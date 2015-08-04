/*
 * model.c++
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */



#include <iostream>
#include <fstream>
#include <string>
#include "./RandomGen.h"
//#include "Utils/RandomGen.h"
#include "Individual.h"
#include "Grid.h"
#include "PhylSimModel.h"
#include "Species.h"


PhylSimModel::PhylSimModel(int X, int Y, int type, bool neutral, bool dd, bool env, unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff)
{
   if (type == 1) {
      m_Global = new GlobalEnvironment(X,Y, type, neutral, dd, env, runs, specRate, dispersalCutoff, densityCutoff);
      m_Local = NULL;
   } else if (type == 2 || type == 3) {
      m_Global = NULL;
      m_Local = new LocalEnvironment(X,Y, type, neutral, dd, env, runs, specRate, dispersalCutoff, densityCutoff);
   }
   m_Dispersal = type;
   m_X_coordinate = X;
   m_Y_coordinate = Y;
}
PhylSimModel::~PhylSimModel(){


}

void PhylSimModel::updateEnvironment(Landscape* environment, unsigned int generation, unsigned int runs) {
   environment->increaseAge();
   environment->reproduce(generation);
   environment->speciation(generation, runs);
}

void PhylSimModel::update(unsigned int runs)
{
   for(unsigned int generation = 1; generation < runs+1; generation++)
   {
      if(generation % 1000 == 0){
         //		std::cout << '\n';
         std::cout << "generation :" <<  generation << '/' << runs << '\n';
      }

      if(m_Dispersal==1){
         //			std::stringstream filename1;
         //			std::stringstream filename2;
         updateEnvironment(m_Global, generation, runs);
         //			if(generation % 1000 == 0 ){
         //				filename1 << "..\\test_out_" << generation << ".txt";
         //				std::ofstream test_matrix(filename1.str().c_str());
         //				for(int ba=0;ba<50;ba++)
         //				{
         //					test_matrix << '\n';
         //
         //					for(int bu=0;bu<50;bu++)
         //					{
         //						test_matrix << Global.individuals[ba][bu].Species->ID <<',';
         //					}
         //				}
         //			}
         //
         //				if (false) {
         //					filename2 << "..\\trait_out_" << generation << ".txt";
         //
         //					std::ofstream trait_matrix(filename2.str().c_str());
         //					for(int ba=0;ba<Global.get_dimensions().first;ba++)
         //					{
         //						trait_matrix << '\n';
         //						for(int bu=0;bu<Global.get_dimensions().second;bu++)
         //						{
         //							trait_matrix << Global.individuals[ba][bu].mean <<',';
         //						}
         //					}
         //				}
         //			}
      }

      else if(m_Dispersal == 3) {
         updateEnvironment(m_Local, generation, runs);

         //			std::stringstream filename1;
         ////			std::stringstream filename2;
         //			if (generation % 1000 == 0)
         //			{
         //
         ////				std::cout << '\n' << "Printing matrix to file" << '\n';
         //
         //				filename1 << "..\\test_out_" << generation << ".txt";
         ////				filename2 << "..\\trait_out_" << generation << ".txt";
         ////
         //				std::ofstream test_matrix(filename1.str().c_str());
         //				for(int ba=0;ba<Local.get_dimensions().first;ba++)
         //				{
         //					test_matrix << '\n';
         //					for(int bu=0;bu<Local.get_dimensions().second;bu++)
         //					{
         //						test_matrix << Local.individuals[ba][bu].Species->ID <<',';
         //					}
         //				}
         //			}
         //				std::ofstream trait_matrix(filename2.str().c_str());
         //				for(int ba=0;ba<Local.get_dimensions().first;ba++)
         //				{
         //					trait_matrix << '\n';
         //					for(int bu=0;bu<Local.get_dimensions().second;bu++)
         //					{
         //						trait_matrix << Local.individuals[ba][bu].mean <<',';
         //					}
         //				}
         //			}
      }
      //		Landscape.reproduce(generation);


      //		std::stringstream filename1;
      //		std::stringstream filename2;
      //		if (generation % 1000 == 0)
      //		{
      //
      //			std::cout << '\n' << "Printing matrix to file" << '\n';
      //
      //			filename1 << "..\\test_out_" << generation << ".txt";
      //			filename2 << "..\\trait_out_" << generation << ".txt";
      //
      //			std::ofstream test_matrix(filename1.str().c_str());
      //			for(int ba=0;ba<Landscape.get_dimensions().first;ba++)
      //			{
      //				test_matrix << '\n';
      //				for(int bu=0;bu<Landscape.get_dimensions().second;bu++)
      //				{
      //					test_matrix << Landscape.individuals[ba][bu].Species->ID <<',';
      //				}
      //			}
      //
      //			std::ofstream trait_matrix(filename2.str().c_str());
      //			for(int ba=0;ba<Landscape.get_dimensions().first;ba++)
      //			{
      //				trait_matrix << '\n';
      //				for(int bu=0;bu<Landscape.get_dimensions().second;bu++)
      //				{
      //					trait_matrix << Landscape.individuals[ba][bu].mean <<',';
      //				}
      //			}
      //		}
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

void PhylSimModel::getclimate(){
   int x = m_Local->get_dimensions().first;
   int y = m_Local->get_dimensions().second;
   std::ofstream temperature_matrix("..\\temperature_out.txt");
   for(int ba=0; ba < x ; ba++){
      temperature_matrix << '\n';

      for(int bu=0;bu < y ;bu++){
         temperature_matrix << m_Local->m_Environment[ba * y + bu].first <<',';
      }}

}


void callModel(int* x, int* y, int* dispersal, int* runs, double* specRate, bool* dens, bool* env, bool* neutral,int* dispersalCutoff, int* densityCutoff,int* seed, int* specOut, double* traitOut, double* neutralOut, double* compOut, double* envOut,  std::string* phyloOut){
   RandomGen ran;
   ran.seedrand(seed[0]);
   PhylSimModel Model(x[0],y[0],dispersal[0], neutral[0], dens[0], env[0], runs[0], specRate[0], dispersalCutoff[0], densityCutoff[0]);

   Model.update(runs[0]);

   if(dispersal[0] == 1){
      int i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            specOut[i] = Model.m_Global->m_Individuals[ba][bu].m_Species->m_ID;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            traitOut[i] = Model.m_Global->m_Individuals[ba][bu].m_Mean;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            envOut[i] = Model.m_Global->m_Environment[ba * y[0] + bu].first;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            neutralOut[i] = Model.m_Global->m_Individuals[ba][bu].m_NeutralMarker;
            i=i+1;
         }
      }

      i = 0;
      for(int ba=0;ba<x[0];ba++){
         for(int bu=0;bu<y[0];bu++){
            compOut[i] = Model.m_Global->m_Individuals[ba][bu].m_CompetitionMarker;
            i=i+1;
         }
      }

      Model.m_Global->m_Phylogeny.prunePhylogeny(Model.m_Global->m_Phylogeny.m_FullPhylogeny);
      phyloOut[0] = Model.m_Global->m_Phylogeny.writePhylogenyR(1, runs[0], Model.m_Global->m_Phylogeny.m_PrunedPhylo);
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
      phyloOut[0] = Model.m_Local->m_Phylogeny.writePhylogenyR(1, runs[0], Model.m_Local->m_Phylogeny.m_PrunedPhylo);
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
