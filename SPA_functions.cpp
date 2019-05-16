// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <iostream>
#include <fstream>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]

double spa_rcpp(NumericVector S, NumericMatrix A, NumericMatrix L, 
                NumericVector Y, int n, double tol, double tol_subtree, double tol_row, 
                NumericMatrix row_sums, 
                NumericMatrix col_sums, bool progress=true) {
  n = 6;
  
  int n_em =  1; //S.nrow();
  int n_ind = A.nrow();
  int n_fd = 1; //Y.ncol();
  
  
  NumericVector path = NumericVector::create(NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  ofstream myfile;
  myfile.open("example.txt");
  myfile << "order \t paths \t value \n";
  double B;
  double resid = 0; 
  
  // layer 0 *****************************************************************
  
  //NumericVector x0(n_ind);
  double x0;
  Progress p(n_ind ^ n, progress);
  for(int j = 0; j < n_ind; j++) {
    p.increment(); // update progress
    
    if (row_sums(0,j) > tol_row) {
      x0 = Y(j);
      B = S(j) * x0;
      if (B > tol) {
        // write to file
        path = NumericVector::create(j+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL); 
        myfile << 1 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
      } else {
        resid += B; 
      }   
    }
    
    // layer 1 *****************************************************************
    if (col_sums(0,j) > tol_subtree) {
      // subtree is above threshold -> continue
      double x1; 
      for (int k = 0; k < n_ind; k++) {
        if (row_sums(1,k) > tol_row) {
          x1 = A(k,j) * x0;
          B = S(k) * x1;
          // B = S(k) * A(k,j) * Y(j);
          if (B > tol) {
            path = NumericVector::create(j+1, k+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
            myfile << 2 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
          } else {
            resid += B; 
          }
        }
        
        // layer 2 *****************************************************************
        if (Progress::check_abort() )
          return -1.0;
        if (col_sums(1,k) > tol_subtree) {
          double x2;
          // subtree is above threshold -> continue
          for (int l = 0; l < n_ind; l++) {
            if (row_sums(2,l) > tol_row) {
              x2 = A(l,k) * x1;
              B = S(l) * x2;
              // B = S(l) * A(l,k) * A(k,j) * Y(j);
              if (B > tol) {
                path = NumericVector::create(j+1, k+1, l+1, NA_REAL, NA_REAL, NA_REAL);
                myfile << 3 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
              } else {
                resid += B; 
              }
            }
            
            // layer 3 *****************************************************************
            if (col_sums(2,l) > tol_subtree) {
              // subtree is above threshold -> continue
              double x3;
              for (int m = 0; m < n_ind; m++) {
                if (row_sums(3,m) > tol_row) {
                  x3 = A(m,l) * x2;
                  B = S(m) * x3;
                  // B = S(m) * A(m,l) * A(l,k) * A(k,j) * Y(j);
                  if (B > tol) {
                    path = NumericVector::create(j+1, k+1, l+1, m+1, NA_REAL, NA_REAL);
                    myfile << 4 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
                  } else {
                    resid += B; 
                  }
                }
                
                // layer 4 *****************************************************************
                if (col_sums(3,m) > tol_subtree) {
                  // subtree is above threshold -> continue
                  double x4;
                  for (int n = 0; n < n_ind; n++) {
                    if (row_sums(4,n) > tol_row) {
                      x4 = A(n,m) * x3;
                      B = S(n) * x4;
                      // B = S(n) * A(n,m) * A(m,l) * A(l,k) * A(k,j) * Y(j);
                      if (B > tol) {
                        path = NumericVector::create(j+1, k+1, l+1, m+1, n+1, NA_REAL);
                        myfile << 5 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
                      } else {
                        resid += B; 
                      }
                    }
                    
                    // layer 5 *****************************************************************
                    if (col_sums(4,n) > tol_subtree) {
                      // subtree is above threshold -> continue
                      double x5;
                      for (int o = 0; o < n_ind; o++) {
                        if (row_sums(5,o) > tol_row) {
                          x5 = A(o,n) * x4;
                          B = S(o) * x5;
                          // B = S(o) * A(o,n) * A(n,m) * A(m,l) * A(l,k) * A(k,j) * Y(j);
                          if (B > tol) {
                            path = NumericVector::create(j+1, k+1, l+1, m+1, n+1, o+1);
                            myfile << 6 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
                          } else {
                            resid += B; 
                          }
                        }
                        // layer 6 add here *************************************
                        // for () .... 
                      }   
                    }   
                  } 
                }
              } 
            }
          }
        }
      }
    }
  }
  myfile.close();
  return(resid);
}






// [[Rcpp::export]]

double spa_recurs(NumericVector S, NumericMatrix A, 
                  double x, NumericVector path, 
                  int L_max, int Layer, int n_ind, int index,
                  double tol, double emissions) {
  if(Layer == L_max) {
    return 1.0;
  }
  cout << Layer << "\t";
  double B;
  for(int i = 0; i < n_ind; i++) {
    cout << "i" << i << "\t";
    x = A(i,index) * x;
    B = S(i) * x;
    path(Layer) = i; 
    if (B > tol) {
      cout << path << "\t" << B << "\t"; 
      // write to file
    }
    emissions += B;
    spa_recurs(S = S, A = A, x = x, path = path, L_max = L_max, Layer = Layer + 1, n_ind = n_ind, 
               index = i, tol = tol, emissions = emissions);
    //return 1.0;
  }
  return emissions; 
}









