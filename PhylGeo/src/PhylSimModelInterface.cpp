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
//#include "Utils/RandomGen.h"
#include "Individual.h"
#include "Grid.h"
#include "PhylSimModel.h"
#include "Species.h"

using namespace Rcpp;



// [[Rcpp::export]]
void callModel(int x, int y, int dispersal, int runs, double specRate, bool dens, 
               bool env, bool mort, bool repro, bool neutral,int dispersalCutoff, 
               int densityCutoff,int seed, IntegerVector specOut, NumericVector traitOut, NumericVector neutralOut,
               NumericVector compOut, NumericVector envOut,  CharacterVector phyloOut) {
   RandomGen ran;
   ran.seedrand(seed);
   PhylSimModel phylSimModel(x, y,dispersal, neutral, dens, env, runs, specRate,
         dispersalCutoff, densityCutoff);

   phylSimModel.update(runs);
   // TODO: Assert that the given vectors specOut, traitOut, neutralOut, compOut, envOut, phyloOut
   // have enough place, i.e. size of them must always be x*y
   if (dispersal == 1) {
      std::cout << "Writing : Species Matrix..." << '\n';
      int indCounter = 0; // individual counter

      // Store the id of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_ID;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environmental trait..." << '\n';
      indCounter = 0;
      // Store the mean of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            traitOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_Mean;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environment..." << '\n';
      indCounter = 0;
      // TODO
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            envOut[indCounter] = phylSimModel.m_Global->m_Environment[i * y + j].first;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Neutral trait..." << '\n';
      indCounter = 0;
      // Store the the neutral marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            neutralOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_NeutralMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Competition trait..." << '\n';
      indCounter = 0;
      // Store the the competition marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            compOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Pruning : Phylogeny..." << '\n';
      phylSimModel.m_Global->m_Phylogeny.prunePhylogeny(phylSimModel.m_Global->m_Phylogeny.m_FullPhylogeny);
      std::cout << "...done" << '\n';

      std::cout << "Writing : Phylogeny..." << '\n';
      std::string phyloPass("\0");
      phyloPass = phylSimModel.m_Global->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Global->m_Phylogeny.m_PrunedPhylo);
      std::cout << "...done" << '\n';

      std::cout << "Passing : Phylogeny..." << '\n';

      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());

      phyloOut[0] = cstr;
      std::cout << "...done" << '\n';
   } else if(dispersal == 2 || dispersal == 3) {
      std::cout << "Writing : Species Matrix..." << '\n';
      int indCounter = 0; // individual counter

      // Store the id of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_ID;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environmental trait..." << '\n';
      indCounter = 0;
      // Store the mean of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            traitOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_Mean;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environment..." << '\n';
      indCounter = 0;
      // TODO
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            envOut[indCounter] = phylSimModel.m_Local->m_Environment[i * y + j].first;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Neutral trait..." << '\n';
      indCounter = 0;
      // Store the the neutral marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            neutralOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_NeutralMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Competition trait..." << '\n';
      indCounter = 0;
      // Store the the competition marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            compOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      phylSimModel.m_Local->m_Phylogeny.prunePhylogeny(phylSimModel.m_Local->m_Phylogeny.m_FullPhylogeny);
      std::string phyloPass = phylSimModel.m_Local->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Local->m_Phylogeny.m_PrunedPhylo);
      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());

      phyloOut[0] = cstr;
   }
}

// [[Rcpp::export]]
void callModelDefault(int runs, IntegerVector specOut, NumericVector traitOut, NumericVector neutralOut,
               NumericVector compOut, NumericVector envOut,  CharacterVector phyloOut) {
   // Set default settings
   int x = 10;
   int y = 10;
   int dispersal = 1;
   double specRate = 0.7;
   bool dens = 0;
   bool env = 0;
   bool mort = 0;
   bool repro = 1;
   bool neutral = 1;
   int dispersalCutoff = 1;
   int densityCutoff = 2;
   int seed = 10;
   
   RandomGen ran;
   ran.seedrand(seed);
   PhylSimModel phylSimModel(x, y,dispersal, neutral, dens, env, runs, specRate,
         dispersalCutoff, densityCutoff);

   phylSimModel.update(runs);
   // TODO: Assert that the given vectors specOut, traitOut, neutralOut, compOut, envOut, phyloOut
   // have enough place, i.e. size of them must always be x*y
   if (dispersal == 1) {
      std::cout << "Writing : Species Matrix..." << '\n';
      int indCounter = 0; // individual counter

      // Store the id of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_ID;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environmental trait..." << '\n';
      indCounter = 0;
      // Store the mean of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            traitOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_Species->m_Mean;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environment..." << '\n';
      indCounter = 0;
      // TODO
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            envOut[indCounter] = phylSimModel.m_Global->m_Environment[i * y + j].first;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Neutral trait..." << '\n';
      indCounter = 0;
      // Store the the neutral marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            neutralOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_NeutralMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Competition trait..." << '\n';
      indCounter = 0;
      // Store the the competition marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            compOut[indCounter] = phylSimModel.m_Global->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Pruning : Phylogeny..." << '\n';
      phylSimModel.m_Global->m_Phylogeny.prunePhylogeny(phylSimModel.m_Global->m_Phylogeny.m_FullPhylogeny);
      std::cout << "...done" << '\n';

      std::cout << "Writing : Phylogeny..." << '\n';
      std::string phyloPass("\0");
      phyloPass = phylSimModel.m_Global->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Global->m_Phylogeny.m_PrunedPhylo);
      std::cout << "...done" << '\n';

      std::cout << "Passing : Phylogeny..." << '\n';

      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());

      phyloOut[0] = cstr;
      std::cout << "...done" << '\n';
   } else if(dispersal == 2 || dispersal == 3) {
      std::cout << "Writing : Species Matrix..." << '\n';
      int indCounter = 0; // individual counter

      // Store the id of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            specOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_ID;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environmental trait..." << '\n';
      indCounter = 0;
      // Store the mean of the species of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            traitOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_Species->m_Mean;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Environment..." << '\n';
      indCounter = 0;
      // TODO
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            envOut[indCounter] = phylSimModel.m_Local->m_Environment[i * y + j].first;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Neutral trait..." << '\n';
      indCounter = 0;
      // Store the the neutral marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            neutralOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_NeutralMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      std::cout << "Writing : Competition trait..." << '\n';
      indCounter = 0;
      // Store the the competition marker of each individual in the array specOut
      for (int i = 0; i < x; i++) {
         for (int j = 0; j < y; j++) {
            compOut[indCounter] = phylSimModel.m_Local->m_Individuals[i][j].m_CompetitionMarker;
            indCounter++;
         }
      }
      std::cout << "...done" << '\n';
      phylSimModel.m_Local->m_Phylogeny.prunePhylogeny(phylSimModel.m_Local->m_Phylogeny.m_FullPhylogeny);
      std::string phyloPass = phylSimModel.m_Local->m_Phylogeny.writePhylogenyR(1, runs, phylSimModel.m_Local->m_Phylogeny.m_PrunedPhylo);
      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str());

      phyloOut[0] = cstr;
   }
}
