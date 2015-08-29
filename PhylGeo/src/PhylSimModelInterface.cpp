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

//’ call main model
//’
//’ @param x ...
//’ @return list of model results
// [[Rcpp::export]]
Rcpp::List callModel(int x, int y, int dispersal, int runs, double specRate, bool dens, 
               bool env, bool neutral, bool mort, int mortStrength, bool repro, int dispersalCutoff, 
               int densityCutoff, int seed) {
                 
   Rcpp::IntegerVector specOut(x*y);                 
   Rcpp::NumericVector traitOut(x*y);
   Rcpp::NumericVector envOut(x*y);
   Rcpp::NumericVector neutralOut(x*y);
   Rcpp::NumericVector compOut(x*y);
   Rcpp::CharacterVector phyloOut = "";   
   
   RandomGen ran;
   ran.seedrand(seed);
   
   std::string tempSaveLoc = "./"; 
   
   PhylSimModel phylSimModel(x, y, dispersal, runs, specRate, dens, 
               env, neutral, mort, mortStrength, repro, dispersalCutoff, 
               densityCutoff, tempSaveLoc);

   phylSimModel.update(runs);
   // TODO: Assert that the given vectors specOut, traitOut, neutralOut, compOut, envOut, phyloOut
   // have enough place, i.e. size of them must always be x*y

    int indCounter = 0; 
 
   if (dispersal == 1) {
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_ID;
            traitOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_Mean;  
            envOut[indCounter] = phylSimModel.m_Global->m_Environment[i * y + j].first;
            neutralOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_NeutralMarker;            
            compOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      phylSimModel.m_Global->m_Phylogeny.prunePhylogeny(phylSimModel.m_Global->m_Phylogeny.m_FullPhylogeny);
      std::string phyloPass("\0");
      phyloPass = phylSimModel.m_Global->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Global->m_Phylogeny.m_PrunedPhylo);
      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());
      phyloOut[0] = cstr;
      
      } else {
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_ID;
            traitOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_Mean;
            envOut[indCounter] = phylSimModel.m_Local->m_Environment[i * y + j].first;
            neutralOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_NeutralMarker;
            compOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      phylSimModel.m_Local->m_Phylogeny.prunePhylogeny(phylSimModel.m_Local->m_Phylogeny.m_FullPhylogeny);
      std::string phyloPass = phylSimModel.m_Local->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Local->m_Phylogeny.m_PrunedPhylo);
      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());
      phyloOut[0] = cstr;
}
   

      
   return Rcpp::List::create(Rcpp::Named("Species") = specOut,
                          Rcpp::Named("EnvTrait") = traitOut,
                          Rcpp::Named("NeutralTrait") = neutralOut,
                          Rcpp::Named("CompetitionTrait") = compOut,
                          Rcpp::Named("Environment") = envOut,
                          Rcpp::Named("Phylogeny") = phyloOut);
}
