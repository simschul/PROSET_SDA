#include <Rcpp.h>
#include <iostream>
#include <fstream>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
void writeToFile (NumericVector path, double x, int n, double tol, std::ostream& outfile) {
  if (x > tol) {
    NumericVector pathToWrite(n, NA_REAL);
    for(int i = 0; i < path.length(); i++) {
      pathToWrite(i) = path(i);
    }
    outfile << "ghelle"; 
    //myfile << 2 << "\t" << path; myfile << "\t"; myfile << B; myfile << "\n";
  }
}


int main () {
  NumericVector path(3);
  double x = 0.4; 
  int n = 5; 
  double tol = 0.1; 
  
  ofstream outfile("test.txt", ios::out); 
  const std::string file = "test.txt"; 
  // outfile.open(file); 
  outfile.open(file.c_str());
  
  writeToFile(path, x, n, tol, outfile);
  outfile.close(); 
  return 0; 
  
  
  
}

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