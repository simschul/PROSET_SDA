#include <Rcpp.h>
#include <fstream>

using namespace Rcpp;
using namespace std;

void writeToFile (ostream& myfile, string text) {
  myfile << text << "\n";
}


// [[Rcpp::export]]
void foo (string text, int n,  string file = "example.txt") {
  ofstream myfile;
  myfile.open(file.c_str());
  for (int i = 0; i < n; i++) {
    writeToFile(myfile, text);
  }
}
