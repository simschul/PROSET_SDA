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

double spa_rcpp(NumericVector S, NumericMatrix A, NumericMatrix L, 
                NumericVector Y, int n, double tol, double tol_subtree, double tol_row, 
                NumericMatrix row_sums, 
                NumericMatrix col_sums, bool progress=true, 
                string file = "example.txt") {
  n = 6;
  
  int n_em =  1; //S.nrow();
  int n_ind = A.nrow();
  int n_fd = 1; //Y.ncol();
  
  
  
  NumericVector path = NumericVector::create(NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  ofstream myfile;
  myfile.open(file.c_str());
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





// [[Rcpp::export]]

double spa_sector(NumericVector S, NumericMatrix A, NumericMatrix L, 
                  NumericVector x, int sector, 
                  int n, double tol, double tol_subtree, double tol_row, 
                  NumericMatrix row_sums, 
                  NumericMatrix col_sums, bool progress=true, 
                  string file = "example.txt") {
  sector = sector - 1; // translate from R (starting with 1) to C++ (with 0)
  n = 6;
  
  int n_em =  1; //S.nrow();
  int n_ind = A.nrow();
  int n_fd = 1; //x.ncol();
  
  NumericVector path = NumericVector::create(NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  double B;
  double resid = 0; 
  
  
  // layer 0 *****************************************************************
  
  //NumericVector x0(n_ind);
  double x0;
  Progress p(n_ind ^ (n - 1), progress);
  x0 = x(sector);
  B = S(sector) * x0;
  if (B > tol) {
    // write to file
    path = NumericVector::create(sector+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL); 
    myfile << 1 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
  } else {
    resid += B; 
  }   
  
  
  // layer 1 *****************************************************************
  double x1; 
  for (int k = 0; k < n_ind; k++) {
    p.increment();
    if (row_sums(1,k) > tol_row) {
      x1 = A(k,sector) * x0;
      B = S(k) * x1;
      // B = S(k) * A(k,sector) * x(sector);
      if (B > tol) {
        path = NumericVector::create(sector+1, k+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
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
          // B = S(l) * A(l,k) * A(k,sector) * x(sector);
          if (B > tol) {
            path = NumericVector::create(sector+1, k+1, l+1, NA_REAL, NA_REAL, NA_REAL);
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
              // B = S(m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
              if (B > tol) {
                path = NumericVector::create(sector+1, k+1, l+1, m+1, NA_REAL, NA_REAL);
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
                  // B = S(n) * A(n,m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
                  if (B > tol) {
                    path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, NA_REAL);
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
                      // B = S(o) * A(o,n) * A(n,m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
                      if (B > tol) {
                        path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, o+1);
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
  
  
  myfile.close();
  return(resid);
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

void writeToFile (std::ofstream& myfile, NumericVector path, double x, int n) {
  // path = NumericVector::create(sector+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  NumericVector pathToWrite(n, NA_REAL); 
  for(int i = 0; i < path.length(); i++) {
    pathToWrite(i) = path(i); 
  }
  
  
  myfile << path.length() << "\t" << pathToWrite; myfile << "\t"; myfile << x; myfile << "\n";
  
}


// [[Rcpp::export]]

double spa_sector_test(NumericVector S, NumericMatrix A, NumericMatrix L, 
                       NumericVector x, int sector, NumericVector F_total, 
                       int n, double tol, double tol_subtree, 
                      bool progress=true, 
                       string file = "example.txt") {
  sector = sector - 1; // translate from R (starting with 1) to C++ (with 0)
  n = 6;
  
  int n_em =  1; //S.nrow();
  int n_ind = A.nrow();
  int n_fd = 1; //x.ncol();
  
  NumericVector path = NumericVector::create(NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  double B;
  double resid = 0; 
  NumericVector yz(n_ind);
  double contSubtree; 
  // layer 0 *****************************************************************
  
  //NumericVector x0(n_ind);
  double x0;
  Progress p(n_ind ^ (n - 1), progress);
  x0 = x(sector);
  B = S(sector) * x0;
  if (B > tol) {
    // write to file
    path = NumericVector::create(sector+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
    myfile << 1 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
    // path = NumericVector::create(sector + 1);
    // writeToFile(myfile, path, B, n); 
    
  } else {
    resid += B; 
  }   
  
  
  // layer 1 *****************************************************************
  double x1; 
  for (int k = 0; k < n_ind; k++) {
    p.increment();
    x1 = A(k,sector) * x0;

    yz(k) = x1; 
    contSubtree = vecMult(F_total, yz); 
    yz(k) = 0; 
    
    B = S(k) * x1;
    if (B > tol) {
      path = NumericVector::create(sector+1, k+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
      myfile << 2 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
    } else {
      resid += B; 
    }
    
    // layer 2 *****************************************************************
    if (contSubtree > tol_subtree) {
      // subtree is above threshold -> continue
      if (Progress::check_abort() )
        return -1.0;
      double x2;
      for (int l = 0; l < n_ind; l++) {
        x2 = A(l,k) * x1;
        
        yz(l) = x2; 
        contSubtree = vecMult(F_total, yz); 
        yz(l) = 0; 
        
        B = S(l) * x2;
        if (B > tol) {
          path = NumericVector::create(sector+1, k+1, l+1, NA_REAL, NA_REAL, NA_REAL);
          myfile << 3 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
        } else {
          resid += B; 
        }
        // layer 3 *****************************************************************
        if (contSubtree > tol_subtree) {
          // subtree is above threshold -> continue
          double x3;
          for (int m = 0; m < n_ind; m++) {
            x3 = A(m,l) * x2;
            
            yz(m) = x3; 
            contSubtree = vecMult(F_total, yz); 
            yz(m) = 0; 
            
            B = S(m) * x3;
            if (B > tol) {
              path = NumericVector::create(sector+1, k+1, l+1, m+1, NA_REAL, NA_REAL);
              myfile << 4 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
            } else {
              resid += B; 
            }
            // layer 4 *****************************************************************
            if (contSubtree > tol_subtree) {
              // subtree is above threshold -> continue
              double x4;
              for (int n = 0; n < n_ind; n++) {
                x4 = A(n,m) * x3;
                
                yz(n) = x4; 
                contSubtree = vecMult(F_total, yz); 
                yz(n) = 0; 
                
                B = S(n) * x4;
                if (B > tol) {
                  path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, NA_REAL);
                  myfile << 5 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
                } else {
                  resid += B; 
                }
                // layer 5 *****************************************************************
                if (contSubtree > tol_subtree) {
                  // subtree is above threshold -> continue
                  double x5;
                  for (int o = 0; o < n_ind; o++) {
                    x5 = A(o,n) * x4;
                    
                    yz(o) = x5; 
                    contSubtree = vecMult(F_total, yz); 
                    yz(o) = 0; 
                    
                    B = S(o) * x5;
                    if (B > tol) {
                      path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, o+1);
                      myfile << 6 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
                    } else {
                      resid += B; 
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
  myfile.close();
  return(resid);
}

// [[Rcpp::export]]

double spa_sector2(NumericVector S, NumericMatrix A, NumericMatrix L, 
                   NumericVector x, int sector, 
                   int n, double tol, double tol_subtree, double tol_row, 
                   NumericMatrix row_sums, 
                   NumericMatrix col_sums, bool progress=true, 
                   string file = "example.txt") {
  sector = sector - 1; // translate from R (starting with 1) to C++ (with 0)
  n = 6;
  
  int n_em =  1; //S.nrow();
  int n_ind = A.nrow();
  int n_fd = 1; //x.ncol();
  
  NumericVector path = NumericVector::create(NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
  ofstream myfile;
  myfile.open(file.c_str());
  myfile << "order \t paths \t value \n";
  double B;
  double resid = 0; 
  
  // layer 0 *****************************************************************
  // direct emissions
  
  double x0;
  Progress p(n_ind ^ (n - 1), progress);
  x0 = x(sector);
  B = S(sector) * x0;
  if (abs(B) > tol) {
    // write to file
    path = NumericVector::create(sector+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL, NA_REAL); 
    myfile << 1 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
  } else {
    resid += B; 
  }   
  
  // layer 1 *****************************************************************
  double x1; 
  for (int k = 0; k < n_ind; k++) {
    p.increment();
    if (row_sums(1,k) > tol_row) {
      x1 = A(k,sector) * x0;
      B = S(k) * x1;
      // B = S(k) * A(k,sector) * x(sector);
      if (abs(B) > tol) {
        path = NumericVector::create(sector+1, k+1, NA_REAL, NA_REAL, NA_REAL, NA_REAL);
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
          // B = S(l) * A(l,k) * A(k,sector) * x(sector);
          if (abs(B) > tol) {
            path = NumericVector::create(sector+1, k+1, l+1, NA_REAL, NA_REAL, NA_REAL);
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
              // B = S(m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
              if (abs(B) > tol) {
                path = NumericVector::create(sector+1, k+1, l+1, m+1, NA_REAL, NA_REAL);
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
                  // B = S(n) * A(n,m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
                  if (abs(B) > tol) {
                    path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, NA_REAL);
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
                      // B = S(o) * A(o,n) * A(n,m) * A(m,l) * A(l,k) * A(k,sector) * x(sector);
                      if (abs(B) > tol) {
                        path = NumericVector::create(sector+1, k+1, l+1, m+1, n+1, o+1);
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
  
  
  myfile.close();
  return(resid);
}



// [[Rcpp::export]]

void makediagonalzero(NumericMatrix mat, int n, int m) {
  for (int i = 0; i < n; i++) { 
    for (int j = 0; j < m; j++) { 
      
      // right and left diagonal condition 
      if (i == j || (i + j + 1) == n) 
        mat(i,j) = 0; 
    } 
  } 
  
  // print resultant matrix 
  // print(mat, n, m); 
} 





