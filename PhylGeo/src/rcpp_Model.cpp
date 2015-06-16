#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

//' @title Model of neutral community assembly
//' @description Runs a simple model of species community assembly under neutral conditions as defined by Hubbel. 
//' @param dim The edge-length of the square metacommunity matrix
//' @param specRate The speciation rate in total speciation events per generation
//' @param seed The random seed (do not change this option if you previously set the seed!)
//' @param runs The number of generations the model runs through
//' @details Be careful with the dimensions you choose. Large grids and many generations may take very long to compute. You can assume to have reached the equilibrium state of the model after dim^2/2.
// [[Rcpp::export]]
NumericMatrix RcppModel( int dim, int specRate, int runs ) {
  Timer timer;
  srand(1500);
  NumericMatrix mat(dim,dim);
  std::fill(mat.begin(), mat.end(), 1);
  unsigned long long counter = 1;
  
  for(int i=0; i<runs; i++){
    
    for(int j=0; j < (dim*dim); j++){
      int xCoord = rand() % dim;
      int yCoord = rand() % dim;
  
      int xCoordsource = rand() % dim;
      int yCoordsource = rand() % dim;
    
      mat(xCoord, yCoord) = mat(xCoordsource, yCoordsource);
    }
    
    for(int k=0; k < specRate; k++){
      int xCoordspec = rand() % dim;
      int yCoordspec = rand() % dim;
      
      mat(xCoordspec, yCoordspec) = counter +1;
      counter += 1;
    }
    if(i % 100 == 0) Rcout << "generation " << i << " of " << runs << '\n';
  }
  
  timer.step("FullRun");

 
  NumericVector time(timer);
  Rcout << "Done!..." << std::accumulate(time.begin(), time.end(), 0.0)/1e9 << " seconds elapsed!" << '\n';
  
  return mat;
}
