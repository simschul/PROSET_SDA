// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>

using namespace Rcpp;
using namespace std;






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





// [[Rcpp::export]]
void sectorFPrec (ofstream & outfile,
    NumericVector path, 
                  NumericMatrix A, NumericVector S,
                  NumericVector yz,
                  NumericVector F_total, double tolSubtree, int n, 
                  NumericVector xlayer,
                  string file) {
  
  //NumericVector xvec(A.nrow());
  double x, xnew; 
  int currentLayer = path.length()+1; 
  if (path.length() < n) {
    // if max. layer not reached yet
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
      
      // Write to file ----
      NumericVector pathToWrite(n);
      for(int j = 0; j < currentLayer; j++) {
        pathToWrite(j) = pathnew(j) + 1;
      }
      //myfile << 1 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
      // cout << currentLayer << "\t"  << pathToWrite << "\t" << B << "\n" ; 
      // End -----
      outfile << "bala"; 
      double contSubtree = calcContSubtree(xnew, 
                                           (int) pathnew(currentLayer-1),
                                           F_total, yz);
      if(contSubtree > tolSubtree) {
        sectorFPrec(outfile, path = pathnew, A = A, S = S, yz = yz,
                    F_total = F_total, tolSubtree = tolSubtree, n, 
                    xlayer, 
                    //myfile, 
                    file);
      } 
    }
  }
}


// [[Rcpp::export]]
void sectorFP (int sector, int n, double x,  
               NumericVector S, 
               NumericMatrix A, 
               NumericVector F_total, 
               double tolSubtree = 0, 
               double tolWrite = 0, 
               string file = "example123.txt") {
  sector--; // R starting with 1
  NumericVector path = NumericVector::create(sector); 
  NumericVector xlayer(n); 
  xlayer(0) = x; 
  NumericVector yz(A.nrow()); 
  
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  sectorFPrec(myfile, path, A, S, yz, F_total, tolSubtree, n, xlayer,
              file); 
}





// #include <Rcpp.h>
// #include <iostream>
// #include <fstream>
// 
// using namespace Rcpp;
// using namespace std;
// 
// // [[Rcpp::export]]
// void writeToFile (NumericVector path, double x, int n, double tol, std::ostream& outfile) {
//   if (x > tol) {
//     NumericVector pathToWrite(n, NA_REAL);
//     for(int i = 0; i < path.length(); i++) {
//       pathToWrite(i) = path(i);
//     }
//     outfile << "ghelle"; 
//     //myfile << 2 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
//   }
// }
// 
// 
// int main () {
//   NumericVector path(3);
//   double x = 0.4; 
//   int n = 5; 
//   double tol = 0.1; 
//   
//   ofstream outfile("test.txt", ios::out); 
//   const std::string file = "test.txt"; 
//   // outfile.open(file); 
//   outfile.open(file.c_str());
//   
//   writeToFile(path, x, n, tol, outfile);
//   outfile.close(); 
//   return 0; 
//   
//   
//   
// }

// // [[Rcpp::export]]
// void word_transform(fstream& infile)
// {
//   infile << "hello";
// }
// 
// int main()
// {
//   fstream infile;
//   infile.open("content.txt");
//   if ( infile.is_open() )
//     word_transform( infile );
//   infile.close();
//   return 0;
// }