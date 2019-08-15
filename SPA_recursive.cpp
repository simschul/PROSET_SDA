// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>

using namespace Rcpp;
using namespace std;


void writeToFile (std::ostream& outfile, NumericVector path, double x, int n, int nmax, double tol) {
  if (x > tol) {
    NumericVector pathToWrite(nmax, NA_REAL);
    for(int i = 0; i < path.length(); i++) {
      pathToWrite(i) = path(i) + 1;
    }
    // outfile << "ghelle" << "\t" << "pathToWrite";
    outfile << n << "\t" << pathToWrite << "\t" << x << "\n";
  }
  
  
  
  //myfile << path.length() << "\t" << pathToWrite; myfile << "\t"; myfile << x; myfile << "\n";
  
}



// [[Rcpp::export]]

double vecMult (NumericVector x, NumericVector y) {
  if(x.length() != y.length()) {
    throw std::range_error("Vector need to have the same length"); 
  }
  double res = 0; 
  for(int i = 0; i < x.length(); i++) {
    res += x(i) * y(i); 
  }
  return res; 
}

// // [[Rcpp::export]]
// void writeToFile (std::ofstream& myfile, NumericVector path, double x, int n) {
//   // path = NumericVector::create(sector+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
//   NumericVector pathToWrite(n, NA_REAL);
//   for(int i = 0; i < path.length(); i++) {
//     pathToWrite(i) = path(i);
//   }
//   myfile << path.length() << "\t" << pathToWrite; myfile << "\t"; myfile << x; myfile << "\n";
// 
// }


// [[Rcpp::export]]
double calcContSubtree (double x, int index, 
                        NumericVector F_total, NumericVector yz)  {
  yz(index) = x; 
  double contSubtree = vecMult(F_total, yz); 
  yz(index) = 0; 
  return contSubtree; 
}

// [[Rcpp::export]]
NumericVector calcContTree (NumericVector path, NumericMatrix A, NumericVector S, double x) {
  int i = path(path.length()-1); 
  int j = path(path.length()-2);
  double xnew = A(i,j) * x;
  double B = S(i) * xnew;
  NumericVector result = NumericVector::create(B, x); 
  return result; 
}





void sectorSPArec (NumericVector path, 
                   NumericMatrix A, NumericVector S,
                   NumericVector yz,
                   NumericVector F_total, 
                   double tolSubtree, double tolWrite,  
                   int n, 
                   NumericVector xlayer, 
                   std::ostream& myfile) {
  
  if (path.length() < n) {
    // if max. layer not reached yet
    double x, xnew; 
    int currentLayer = path.length()+1; 
    // add additional layer to path, copy previous path
    NumericVector pathnew(currentLayer);
    for(int j = 0; j < (currentLayer-1); j++) {
      pathnew(j) = path(j);
    }
    for(int i = 0; i < A.nrow(); i++) {
      // loop through all industries
      pathnew(currentLayer-1) = i; // update path
      x = xlayer(currentLayer-2); // take the output from the previous layer
      xnew = A(i,pathnew(currentLayer-2)) * x; // calc output at current layer
      double B = S(i) * xnew;
      xlayer(currentLayer-1) = xnew; // store output of that layer
      if(i != pathnew(0)) {
        // only write to file if the regarded sector is not the emitting sector (to avoid double counting)
        writeToFile(myfile, pathnew, B, currentLayer, n, tolWrite);
      }
      
      double contSubtree = calcContSubtree(xnew, 
                                           (int) pathnew(currentLayer-1),
                                           F_total, yz);
      if(contSubtree > tolSubtree) {
        Rcout << "*"; 
        sectorSPArec(path = pathnew, A = A, S = S, yz = yz,
                     F_total = F_total, tolSubtree = tolSubtree, 
                     tolWrite = tolWrite, 
                     n, 
                     xlayer, 
                     myfile);
      } 
    }
  }
}

void sectorSPArec2 (NumericVector path, 
                    NumericMatrix A, NumericVector S,
                    NumericVector yz,
                    NumericVector F_total, 
                    double tolSubtree, double tolWrite,  
                    int n, 
                    NumericVector xlayer, 
                    int sector, 
                    std::ostream& myfile) {
  
  if (path.length() < n) {
    // if max. layer not reached yet
    double x, xnew;
    bool containsIndex = false; 
    int currentLayer = path.length()+1;
    // add additional layer to path, copy previous path
    NumericVector pathnew(currentLayer);
    for(int j = 0; j < (currentLayer-1); j++) {
      if (path(j) == sector) containsIndex = true; 
      pathnew(j) = path(j);
    }
    for(int i = 0; i < A.nrow(); i++) {
      if (i == sector) containsIndex = true; 
      // loop through all industries
      pathnew(currentLayer-1) = i; // update path
      x = xlayer(currentLayer-2); // take the output from the previous layer
      xnew = A(i,pathnew(currentLayer-2)) * x; // calc output at current layer
      double B = S(i) * xnew;
      xlayer(currentLayer-1) = xnew; // store output of that layer
      if (containsIndex) {
        // only write to file if the regarded sector is part of the path
        writeToFile(myfile, pathnew, B, currentLayer, n, tolWrite);
      }
      
      double contSubtree = calcContSubtree(xnew, 
                                           (int) pathnew(currentLayer-1),
                                           F_total, yz);
      if(contSubtree > tolSubtree) {
        containsIndex = false;        
        Rcout << "*"; 
        sectorSPArec(path = pathnew, A = A, S = S, yz = yz,
                     F_total = F_total, tolSubtree = tolSubtree, 
                     tolWrite = tolWrite, 
                     n, 
                     xlayer, 
                     myfile);
      } 
    }
  }
}


// [[Rcpp::export]]
void sectorSPA (int sector, int n, double x,  
                NumericVector S, 
                NumericMatrix A, 
                NumericVector F_total, 
                double tolSubtree = 0, 
                double tolWrite = 0, 
                string file = "example.txt") {
  sector--; // R starting with 1
  NumericVector path = NumericVector::create(sector); 
  NumericVector xlayer(n); 
  xlayer(0) = x; 
  NumericVector yz(A.nrow());
  double B = S(sector) * x; 
  
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  writeToFile(myfile, path, B, 1, n, tolWrite); 
  sectorSPArec(path, A, S, yz, F_total, tolSubtree, tolWrite, n, xlayer,
               myfile); 
  Rcout << "\n";
}

// [[Rcpp::export]]
void sectorSPA2 (int sector, int n, NumericVector Y,  
                 NumericVector S, 
                 NumericMatrix A, 
                 NumericVector F_total, 
                 double tolSubtree = 0, 
                 double tolWrite = 0, 
                 string file = "example.txt") {
  sector--; // R starting with 1
  NumericVector path(1); 
  
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  
  NumericVector yz(A.nrow());
  NumericVector xlayer(n); 
  double contSubtree; 
  for (int i = 0; i < Y.length(); i++) {
    //if (Progress::check_abort()) return; 
    contSubtree = calcContSubtree(Y(i), i,
                                  F_total, yz);
    if (contSubtree > tolSubtree) {
      xlayer(0) = Y(i); 
      path(0) = i; 
      Rcout << i + 1 << " "; 
      sectorSPArec2(path, A, S, yz, F_total, tolSubtree, tolWrite, n, xlayer, sector,
                    myfile);   
    }
  }
  Rcout << "\n";
}


