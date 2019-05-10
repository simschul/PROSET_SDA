#include <Rcpp.h>
#include <iostream>
//#include <ofstream>
#include <fstream>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]


void myMmult(NumericMatrix A, NumericMatrix B, double tol) {
  
  int dim1 = A.nrow();
  int dim2 = A.ncol();
  int dim3 = B.nrow();
  // int dim1 = 4; 
  // int dim2 = 4; 
  // int dim3 = 4; 
  
  // double array[50][50][50];
  // NumericVector vec(dim1*dim2*dim3);
  // List values;
  // List paths;
  
  double value;
  NumericVector path(3);
  
  ofstream myfile;
  myfile.open("example.txt");
  //int count = 0;
  for(int i = 0; i < dim1; i++) {
    for(int j = 0; j < dim2; j++) {
      if(A(i,j) != 0) {
        for(int k = 0; k < dim3; k++) {
          //array[i][j][k] = A(i,j) * B(j,k);
          value = A(i,j) * B(j,k);
          if(value > tol) {
            //vec(count) = A(i,j) * B(j,k);
            path(0) = i+1; path(1) = j+1; path(2) = k+1;
            myfile << path; 
            myfile << "\t";
            myfile << value;
            myfile << "\n";
            //   //result(count) = value;
            //   values.push_back(value);
            //   paths.push_back(path);
            //count++;  
          } // endif 
        } // k
      } // endif
      
    } // j
  } // i
  myfile.close();
  // List result;
  // result["values"] = values; 
  // result["paths"] = paths;
  // result["array"] = array;
  // return NULL;
}


