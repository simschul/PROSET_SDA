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

//// [[Rcpp::export]]

// int sectorFP2 (NumericVector path, double x, 
//                         NumericMatrix A, NumericVector S,
//                         NumericVector yz,
//                         NumericVector F_total, double tolSubtree, int n, 
//                         string file = "example.txt") {
// 
//   
//   // if max. layer already reached, stop
//   if(path.length() >= n-1) return 0;
//   
//   // if not: check if last element of path (= emitting industry) is the last industry
//   int i = path(path.length()-1);
//   if(i < ( A.nrow() - 1 )) {
//     NumericVector pathnew(path.length()+1);
//     for(int j = 0; j < path.length(); j++) {
//       pathnew(j) = path(j);
//     }
//     pathnew(pathnew.length()-1)++; 
//     calcContTree(path = pathnew, A, S, x); 
//   } else {
//     // next layer
//     path.push_back(0);
//   }
//   
//   NumericVector xvec(A.nrow());
//   
//   if (path.length() < n) {
//     // if max. layer not reached yet
//     NumericVector pathnew(path.length()+1);
//     for(int j = 0; j < path.length(); j++) {
//       pathnew(j) = path(j);
//     }
//     //double xnew; 
//     for(int i = 0; i < A.nrow(); i++) {
//       cout << "--------------------------------------------------------\n"; 
//       // loop through all industries
//       pathnew(pathnew.length()-1) = i; // extended path
//       xvec(i) = A(i,pathnew(pathnew.length()-2)) * x;
//       double B = S(i) * xvec(i);
//       
//       //double xnew = A(i,pathnew(pathnew.length()-2)) * x;
//       //double B = S(i) * xnew;
//       
//       // pathnew.push_back(i); // extended path
//       cout << pathnew << "\t i = " << i << "\t x = " << x << "\t xnew = " << xvec << "\t B = " << B << "\n"; 
//       
//       NumericVector pathToWrite(n);
//       for(int j = 0; j < pathnew.length(); j++) {
//         pathToWrite(j) = pathnew(j) + 1;
//       }
//       
//       //cout << pathnew.length() << "\t"  << pathToWrite; 
//       //cout << "\t";  cout << B; cout << "\t" << pathnew << "\n";
//       double contSubtree = calcContSubtree(xvec(i), (int) pathnew(pathnew.length()-1),
//                                            F_total, yz);
//       if(contSubtree > tolSubtree) {
//         NumericVector pathnew2 = clone(pathnew);
//         //double xnew2 = xnew; 
//         //cout << "xnew2 = " <<  xnew2<< "\t x = " << x <<  "\n";
//         
//         sectorFP(path = pathnew2, x = xvec(i), A = A, S = S, yz = yz,
//                  F_total = F_total, tolSubtree = tolSubtree, n, file);
//       } 
//     }
//   }
//   
//     return xvec; 
//   
//   
// }




// [[Rcpp::export]]

NumericVector sectorFP (NumericVector path, double x, 
               NumericMatrix A, NumericVector S,
               NumericVector yz,
               NumericVector F_total, double tolSubtree, int n, 
               NumericVector xlayer, 
               string file = "example.txt") {
  //if(path.length() >= n-1) return 0;
  // int i = path(path.length()-1); 
  // 
  // if(i < A.nrow()-1) {
  //   path(path.length()-1)++; 
  // } else {
  //   // next layer
  //   path.push_back(0); 
  // }
  // 
  // double xnew = A(i,path(path.length()-1)) * x;
  // double B = S(i) * xnew;
  // 
  // cout << path.length() <<  "\t"  << path; cout << "\t"; cout << B; cout << "\n";
  // 
  // double contSubtree = calcContSubtree(xnew, (int) path(path.length()-1),
  //                                      F_total, yz);
  // 
  // if(contSubtree > tolSubtree && path.length() < n) {
  //   // cout << "new round \n";
  //   sectorFP(path = path, x = xnew, A = A, S = S, yz = yz,
  //            F_total = F_total, tolSubtree = tolSubtree, n, file);
  // } else {
  //   // return 0; 
  // }
  //
  //NumericVector pathnew = clone(path);
  int currentLayer; 
  bool jumpBack = false; 
  NumericVector xvec(A.nrow());
  
  if (path.length() < n) {
    // if max. layer not reached yet
    NumericVector pathnew(path.length()+1);
    for(int j = 0; j < path.length(); j++) {
      pathnew(j) = path(j);
    }
    currentLayer = pathnew.length(); 
    //double xnew; 
    for(int i = 0; i < A.nrow(); i++) {
      if(jumpBack) {
        cout << "jump bakc \n"; 
         x = xlayer(pathnew.length()-2); 
      }
   //   cout << "--------------------------------------------------------\n"; 
      // loop through all industries
      pathnew(pathnew.length()-1) = i; // extended path
      //x = xlayer(pathnew.length()-2); 
      x = xlayer(pathnew.length()-2); 
      xvec(i) = A(i,pathnew(pathnew.length()-2)) * x;
      double B = S(i) * xvec(i);
      xlayer(pathnew.length()-1) = xvec(i); 
      //xlayer(pathnew.length()-1) = xvec(i);
      //double xnew = A(i,pathnew(pathnew.length()-2)) * x;
      //double B = S(i) * xnew;
      
      // pathnew.push_back(i); // extended path
      // cout << pathnew << "\t xlayer = " << xlayer <<  "\t i = " << i << "\t x = " << 
      //   x << "\t xnew = " << xvec(i) << "\t B = " << B << "\n"; 
      
      // cout << "pathlenght = " <<  pathnew.length() << "\t xlayer " << xlayer(pathnew.length()-2) << "\n"; 
      NumericVector pathToWrite(n);
      for(int j = 0; j < pathnew.length(); j++) {
        pathToWrite(j) = pathnew(j) + 1;
      }
      
      cout << pathnew.length() << "\t"  << pathToWrite;
      cout << "\t";  cout << B << "\n" ; //cout << "\t" << pathnew << "\n";
      double contSubtree = calcContSubtree(xvec(i), (int) pathnew(pathnew.length()-1),
                                           F_total, yz);
      if(contSubtree > tolSubtree) {
        // cout << "path = " << pathnew << "\t contSubtree = " << contSubtree << "\t tol = " << tolSubtree << "\n"; 
        NumericVector pathnew2 = clone(pathnew);
        //double xnew2 = xnew; 
        //cout << "xnew2 = " <<  xnew2<< "\t x = " << x <<  "\n";
        
        sectorFP(path = pathnew2, x = xvec(i), A = A, S = S, yz = yz,
                 F_total = F_total, tolSubtree = tolSubtree, n, 
                 xlayer, 
                 file);
      } else {
        //cout << i << "=" << A.nrow()-1 << "\n" ; 
        if(i == A.nrow()-1){
          x = xlayer(pathnew.length()-2); 
        } 
      }
    }
    return xvec; 
  }
  
  
  
  // for(int i = 0; i < A.nrow(); i++) {
  //   double xnew = A(i,path(path.length()-1)) * x;
  //   double B = S(i) * xnew;
  //   //writeToFile(myfile, path = path, x = B, n = n);
  //   // ofstream myfile;
  //   // myfile.open(file.c_str(), std::ofstream::app);
  // 
  //   NumericVector pathnew = path;
  //   pathnew.push_back(i);
  // 
  //   // NumericVector pathnew = clone(path);
  //   // pathnew.push_back(i);
  //   //
  //   NumericVector pathToWrite(n);
  //   for(int j = 0; j < pathnew.length(); j++) {
  //     pathToWrite(j) = pathnew(j) + 1;
  //     //cout << j << " ";
  //   }
  //   //cout << "\n";
  //   // myfile << path.length() << "\t" << pathToWrite; myfile << "\t"; myfile << x; myfile << "\n";
  //   cout << pathnew.length() << "\t" <<  "\t"  << pathToWrite; cout << "\t"; cout << B; cout << "\n";
  // 
  //   double contSubtree = calcContSubtree(xnew, (int) pathnew(pathnew.length()-1),
  //                                        F_total, yz);
  // 
  //   if(contSubtree > tolSubtree && pathnew.length() < n) {
  //     // cout << "new round \n";
  //     sectorFP(path = pathnew, x = xnew, A = A, S = S, yz = yz,
  //              F_total = F_total, tolSubtree = tolSubtree, n, file);
  //   } 
  // }
  // // return 1;
}


// int main () {
//   string file = "example.txt";
//   ofstream myfile;
//   myfile.open(file.c_str());
//   //writeToFile(myfile, NumericVector(4), 0.1, 6);
//   //myfile.open(file);
//   myfile.close();
//   return 0;
// }

// // [[Rcpp::export]]
// void callSectorFP (NumericVector path, double x, 
//                    NumericMatrix A, NumericVector S, 
//                    NumericVector yz, 
//                    NumericVector F_total, double tolSubtree, int n)  {
//   string file = "example.txt";
//   ofstream myfile;
//   myfile.open(file.c_str());
//   
//   sectorFP(myfile, path, x, A, S, yz, F_total, tolSubtree, n);
//   
// }
