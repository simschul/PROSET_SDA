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
    outfile << n << "\t" << pathToWrite << "\t" << x << "\n";
  }
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


// [[Rcpp::export]]
double calcContSubtree (double x, int index, 
                        NumericVector F_total, NumericVector yz)  {
  yz(index) = x; 
  double contSubtree = vecMult(F_total, yz); 
  yz(index) = 0; 
  return contSubtree; 
}

// [[Rcpp::export]]
NumericVector calcContTree (NumericVector path, NumericMatrix A, 
                            NumericVector S, double x) {
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
                   std::ostream& myfile, 
                   std::ostream& residfile) {
  
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
        if(B <= tolWrite){
          residfile << B << "\t" << "tolWrite" << "\n";
        }
      } 
      
      double contSubtree = calcContSubtree(xnew, 
                                           (int) pathnew(currentLayer-1),
                                           F_total, yz);
      if (contSubtree > tolSubtree) {
        Rcout << "*"; 
        sectorSPArec(path = pathnew, A = A, S = S, yz = yz,
                     F_total = F_total, tolSubtree = tolSubtree, 
                     tolWrite = tolWrite, 
                     n, 
                     xlayer, 
                     myfile, 
                     residfile);
      } else {
        contSubtree = contSubtree - B;  
        residfile << contSubtree << "\t"<< "tolSubtree \n"; 
      } 
    }
  }
}




// [[Rcpp::export]]
double sectorSPA (int sector, int n, double x,  
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
  
  ofstream residfile; 
  string file2 = "resid.txt"; 
  residfile.open(file2.c_str());
  
  // double resid = 0.0; 
  sectorSPArec(path, A, S, yz, F_total, tolSubtree, tolWrite, n, xlayer,
               myfile, residfile);
  Rcout << "\n";
  // return resid; 
}



