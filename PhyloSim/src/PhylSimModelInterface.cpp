/*
 * Author: Betim Musa 
 * This is the main interface between the phylogeny simulation (written in C++) and R.
 */


#include <Rcpp.h>

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include "./RandomGen.h"
#include "Individual.h"
#include "Grid.h"
#include "PhylSimModel.h"
#include "Species.h"

using namespace Rcpp;

//' Core phylosim model
//' @export
// [[Rcpp::export]]
List callModel(int x, int y, int dispersal, IntegerVector runs, double specRate, bool dens, bool env, bool neutral, bool mort, int mortStrength, bool repro, int dispersalCutoff, int densityCutoff, int seed, double envStrength, double compStrength, int fission, double redQueen, double redQueenStrength, int protracted, NumericVector airmatR, NumericVector soilmatR) {
                
 
   
   RandomGen ran;
   ran.seedrand(seed);
   
   std::string tempSaveLoc = "./"; 

    int steps =  runs.length();
    int runcount = 0;
    
    std::vector<double> airmat( airmatR.begin(), airmatR.end() ) ;
    std::vector<double> soilmat( soilmatR.begin(), soilmatR.end() ) ;

    Rcpp::List outList = Rcpp::List::create();

   PhylSimModel phylSimModel(x, y, dispersal, runs[steps-1], specRate, dens, env, neutral, mort, mortStrength, repro, dispersalCutoff, densityCutoff, tempSaveLoc, envStrength, compStrength, fission, redQueen, redQueenStrength, protracted, airmat, soilmat);


  for (int step = 0; step < steps; step++) {
    
     Rcpp::IntegerVector specOut(x*y);                 
     Rcpp::NumericVector traitOut(x*y);
     Rcpp::NumericVector envOut(x*y);
     Rcpp::NumericVector neutralOut(x*y);
     Rcpp::NumericVector compOut(x*y);
     Rcpp::CharacterVector phyloOut = "";   
    
    
    int runNow = runs[step]-runcount;
  //  std::cout << "Run model for" <<  runNow << "steps \n";
    phylSimModel.update(runNow);

    runcount = runs[step];
    
    int indCounter = 0; 
 
     if (dispersal == 1) {
        for (int i = 0; i < x; i++) {
           for (int j = 0; j < y; j++) {
              specOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_ID;
              traitOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Mean;  
              envOut[indCounter] = phylSimModel.m_Global->m_Environment[i * y + j].first;
              neutralOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_NeutralMarker;            
              compOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_CompetitionMarker;
              indCounter++;
           }
        }
        phylSimModel.m_Global->m_Phylogeny.prunePhylogeny(runcount);
        std::string phyloPass("\0");

        phyloPass = phylSimModel.m_Global->m_Phylogeny.writePhylogenyR(1, phylSimModel.m_Global->m_Phylogeny.m_PrunedPhylo);

        char * cstr = new char [phyloPass.length()+1];
        std::strcpy (cstr, phyloPass.c_str());
        phyloOut[0] = cstr;
        delete[] cstr;
        
        } else {
        for (int i = 0; i < x; i++) {
           for (int j = 0; j < y; j++) {
              specOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_ID;
              traitOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Mean;
              envOut[indCounter] = phylSimModel.m_Local->m_Environment[i * y + j].first;
              neutralOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_NeutralMarker;
              compOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_CompetitionMarker;
              indCounter++;
           }
        }
        phylSimModel.m_Local->m_Phylogeny.prunePhylogeny(runcount);

       std::string phyloPass = phylSimModel.m_Local->m_Phylogeny.writePhylogenyR(1, phylSimModel.m_Local->m_Phylogeny.m_PrunedPhylo);


        char * cstr = new char [phyloPass.length()+1];
        std::strcpy (cstr, phyloPass.c_str());
        phyloOut[0] = cstr;
        delete[] cstr;
        }
        
        Rcpp::List listResults = Rcpp::List::create(Rcpp::Named("Species") = specOut,
                          Rcpp::Named("EnvTrait") = traitOut,
                          Rcpp::Named("NeutralTrait") = neutralOut,
                          Rcpp::Named("CompetitionTrait") = compOut,
                          Rcpp::Named("Environment") = envOut,
                          Rcpp::Named("Phylogeny") = phyloOut);
                          
        outList.push_back(listResults);
  }

      
   return outList;
}
