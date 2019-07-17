// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>

using namespace Rcpp;
using namespace std;


double logMean (double x, double y) {
  if(x > 0 && y > 0 && x != y) {
    return ((x - y) / (log(x) - log(y)));   
  } else {
    return 0; 
  }
}


double vecProd (NumericVector x) {
  double res = 1; 
  for(int i = 0; i < x.length(); i++) {
    res *= x(i); 
  }
  return res; 
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

NumericVector SDA_lmdi (NumericVector y0, NumericVector y1) {
  if(y0.length() != y1.length()) {
    throw std::range_error("Vectors are not of the same length!");
  } 
  int n = y0.length();
  double prod0 = vecProd(y0); 
  double prod1 = vecProd(y1); 
  double log_mean = logMean(prod1, prod0); 
  
  NumericVector result(n); 
  for(int i = 0; i < n; i++) {
    result(i) = log_mean * log ( y1(i) / y0(i) ); 
  }
  return result; 
}


// [[Rcpp::export]]

List SPD (List table0, List table1, List indices) {
  int n = indices.length();
  int n_comp = table0.length(); 
  
  NumericMatrix a0 = as<NumericMatrix>(table0["A"]); 
  NumericMatrix a1 = as<NumericMatrix>(table1["A"]); 
  
  List spd(n); 
  for(int ilayer = 0; ilayer < n; ilayer++) {
    NumericMatrix indexMatrix = as<NumericMatrix>(indices[ilayer]);
    if (ilayer > 0) {
      table0.insert(1, a0);
      table1.insert(1, a1);
    }
    n_comp = table0.length(); 
    NumericMatrix results(indexMatrix.nrow(), n_comp);
    for(int ipath = 0; ipath < indexMatrix.nrow(); ipath++) {
      NumericVector y0(n_comp);
      NumericVector y1(n_comp);
      for(int icomp = 0; icomp < n_comp; icomp++) {
        NumericMatrix x0 = as<NumericMatrix>(table0[icomp]); 
        y0(icomp) = x0(indexMatrix(ipath,icomp), indexMatrix(ipath,icomp+1));
        NumericMatrix x1 = as<NumericMatrix>(table1[icomp]);
        y1(icomp) = x1(indexMatrix(ipath,icomp), indexMatrix(ipath,icomp+1));
        
      } 
      results(ipath,_) = SDA_lmdi(y0, y1); 
      //cout << y0 << "\t" << y1 << "\n"; 
    }
    // append to list
    spd(ilayer) = results; 
  }
  return spd; 
}
  
  
  
  
  

