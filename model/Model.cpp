/*
 * model.c++
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include "Model.h"

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <Utils/RandomGen.h>
#include "Individual.h"
#include <Grid.h>
#include <sstream>

#include "Species.h"


model::model(int X, int Y, int type, bool neutral, bool dd, bool env, bool mort, bool repro, unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff, unsigned int mortalityStrength, std::string location)
{
   if (type == 1) {
      m_Global = new GlobalEnvironment(X,Y, type, neutral, dd, env, mort, repro, runs, specRate, dispersalCutoff, densityCutoff, mortalityStrength);
      m_Local = NULL;
   } else if (type == 2 || type == 3) {
      m_Global = NULL;
      m_Local = new LocalEnvironment(X,Y, type, neutral, dd, env, mort, repro, runs, specRate, dispersalCutoff, densityCutoff, mortalityStrength);
   }
   m_Dispersal = type;
   m_X_coordinate = X;
   m_Y_coordinate = Y;
}
model::~model(){


}

void model::updateEnvironment(Landscape* environment, unsigned int generation, unsigned int runs) {
   environment->increaseAge();
   environment->reproduce(generation);
   environment->speciation(generation, runs);
}

void model::update(unsigned int runs, std::string location)
{
   for(unsigned int generation = 1; generation < runs+1; generation++)
   {
      if(generation % 1000 == 0){
         //		std::cout << '\n';
         std::cout << "generation :" <<  generation << '/' << runs << '\n';
      }

      if(m_Dispersal==1)
      {

         updateEnvironment(m_Global, generation, runs);

			if(generation >= runs/2 && generation % 1000 == 0 )
			{
				std::stringstream filename1;

				filename1 << location << "/specOut" << generation << ".txt";
				std::ofstream species(filename1.str().c_str());
				for(int ba=0;ba<m_Global->m_Xdimensions ;ba++)
				{
					species << '\n';

					for(int bu=0;bu<m_Global->m_Ydimensions;bu++)
					{
						species << m_Global->m_Individuals[ba][bu].m_Species->m_ID <<',';
					}
				}

			std::stringstream filename2;
			filename2 << location << "/phyloOut" << generation << ".txt";

			std::ofstream phylogeny(filename2.str().c_str());
			m_Global->m_Phylogeny.prunePhylogeny(m_Global->m_Phylogeny.m_FullPhylogeny);
			phylogeny << m_Global->m_Phylogeny.writePhylogenyR(1, generation, m_Global->m_Phylogeny.m_PrunedPhylo);
		}
	}


      else if(m_Dispersal == 3) {
         updateEnvironment(m_Local, generation, runs);

         if(generation >= runs/2 && generation % 1000 == 0 )
         			{
         				std::stringstream filename1;

         				filename1 << location << "/specOut" << generation << ".txt";
         				std::ofstream species(filename1.str().c_str());
         				for(int ba=0;ba<m_Local->m_Xdimensions ;ba++)
         				{
         					species << '\n';

         					for(int bu=0;bu<m_Local->m_Ydimensions;bu++)
         					{
         						species << m_Local->m_Individuals[ba][bu].m_Species->m_ID <<',';
         					}
         				}

         			std::stringstream filename2;
         			filename2 << location << "/phyloOut" << generation << ".txt";

         			std::ofstream phylogeny(filename2.str().c_str());
         			m_Local->m_Phylogeny.prunePhylogeny(m_Local->m_Phylogeny.m_FullPhylogeny);
         			phylogeny << m_Local->m_Phylogeny.writePhylogenyR(1, generation, m_Local->m_Phylogeny.m_PrunedPhylo);
         		}
      }
    }
}

//! Public class method to increase age of all individuals by one
/*!
  \return void
 */


//void model::get_data(){
//
//		std::ofstream test_matrix("..\\test_out.txt");
//		for(int ba=0;ba<Landscape.get_dimensions().first;ba++){
//			test_matrix << '\n';
//
//		for(int bu=0;bu<Landscape.get_dimensions().second;bu++){
//			test_matrix << Landscape.individuals[ba][bu].Species->ID <<',';
//		}}
//
//}

//void model::getclimate(){
//   int x = m_Local->get_dimensions().first;
//   int y = m_Local->get_dimensions().second;
//   std::ofstream temperature_matrix("..\\temperature_out.txt");
//   for(int ba=0; ba < x ; ba++){
//      temperature_matrix << '\n';
//
//      for(int bu=0;bu < y ;bu++){
//         temperature_matrix << m_Local->m_Environment[ba * y + bu].first <<',';
//      }}
//
//}


void callModel(int* x, int* y, int* dispersal, int* runs, double* specRate,
		   bool* dens, bool* env, bool* neutral, bool* mort,unsigned int* mortStrength, bool* repro,
		   int* dispersalCutoff, int* densityCutoff, int* seed, char** saveLocation,
		   int* specOut, double* traitOut, double* neutralOut, double* compOut, double* envOut, char** phyloOut){
//   int mortStr = 50;
	RandomGen ran;
   ran.seedrand(seed[0]);
   model Model(x[0],y[0],dispersal[0], neutral[0], dens[0], env[0], mort[0], repro[0], runs[0], specRate[0], dispersalCutoff[0], densityCutoff[0], mortStrength[0], saveLocation[0]);

   Model.update(runs[0], saveLocation[0]);

   if(dispersal[0] == 1){
	  std::cout << "Writing : Species Matrix" << '\n';
      int i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            specOut[i] = Model.m_Global->m_Individuals[ba][bu].m_Species->m_ID;
            i=i+1;
         }
      }
      std::cout << "done" << '\n';

      std::cout << "Writing : Environmental trait" << '\n';
      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            traitOut[i] = Model.m_Global->m_Individuals[ba][bu].m_Mean;
            i=i+1;
         }
      }
      std::cout << "done" << '\n';

      std::cout << "Writing : Environment" << '\n';
      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            envOut[i] = Model.m_Global->m_Environment[ba * y[0] + bu].first;
            i=i+1;
         }
      }
      std::cout << "done" << '\n';

      std::cout << "Writing : Neutral trait" << '\n';
      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            neutralOut[i] = Model.m_Global->m_Individuals[ba][bu].m_NeutralMarker;
            i=i+1;
         }
      }
      std::cout << "done" << '\n';

      std::cout << "Writing : Competition trait" << '\n';
      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            compOut[i] = Model.m_Global->m_Individuals[ba][bu].m_CompetitionMarker;
            i=i+1;
         }
      }
      std::cout << "done" << '\n';

      std::cout << "Pruning : Phylogeny" << '\n';
      Model.m_Global->m_Phylogeny.prunePhylogeny(Model.m_Global->m_Phylogeny.m_FullPhylogeny);
      std::cout << "done" << '\n';

      std::cout << "Writing : Phylogeny" << '\n';
     std::string phyloPass("\0");
     phyloPass = Model.m_Global->m_Phylogeny.writePhylogenyR(1, runs[0], Model.m_Global->m_Phylogeny.m_PrunedPhylo);
      std::cout << "done" << '\n';

      std::cout << "Passing : Phylogeny" << '\n';

      char * cstr = new char [phyloPass.length()+1];
      std::strcpy (cstr, phyloPass.c_str()); //cstring is necissary for passing the phylogeny to R without errors

      phyloOut[0] = cstr;
      std::cout << "done" << '\n';
   }

   else if(dispersal[0] == 2 ||dispersal[0] == 3){
      int i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            specOut[i] = Model.m_Local->m_Individuals[ba][bu].m_Species->m_ID;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            traitOut[i] = Model.m_Local->m_Individuals[ba][bu].m_Mean;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            envOut[i] = Model.m_Local->m_Environment[ba * y[0] + bu].first;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            neutralOut[i] = Model.m_Local->m_Individuals[ba][bu].m_NeutralMarker;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            compOut[i] = Model.m_Local->m_Individuals[ba][bu].m_CompetitionMarker;
            i=i+1;
         }
      }

      Model.m_Local->m_Phylogeny.prunePhylogeny(Model.m_Local->m_Phylogeny.m_FullPhylogeny);
      std::string phyloPass = Model.m_Local->m_Phylogeny.writePhylogenyR(1, runs[0], Model.m_Local->m_Phylogeny.m_PrunedPhylo);
      char * cstr = new char [phyloPass.length()+1];
       std::strcpy (cstr, phyloPass.c_str()); //cstring is necissary for passing the phylogeny to R without errors

           phyloOut[0] = cstr;
   }
}




//void model::gettraits(){
//	std::ofstream trait_matrix("..\\trait_out.txt");
//		for(int ba=0;ba<Landscape.get_dimensions().first;ba++){
//			trait_matrix << '\n';
//
//		for(int bu=0;bu<Landscape.get_dimensions().second;bu++){
//			trait_matrix << Landscape.individuals[ba][bu].mean <<',';
//		}}
//
//}
