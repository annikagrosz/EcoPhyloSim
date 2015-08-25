/*
 * model.h
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 */

#ifndef PHYLSIMMODEL_H_
#define PHYLSIMMODEL_H_


#include <string>

#include "Grid.h"

class PhylSimModel {
 public:
   int m_Dispersal;
   int m_X_coordinate;
   int m_Y_coordinate;
   GlobalEnvironment* m_Global;
   LocalEnvironment* m_Local;

   PhylSimModel(int x, int y, int dispersal, int runs, double specRate, bool dens, 
               bool env, bool neutral, bool mort, int mortStrength, bool repro, int dispersalCutoff, 
               int densityCutoff,std::string saveLocation);
   ~PhylSimModel();
   void get_data();
   void getclimate();
   void update(unsigned int runs);
   void gettraits();

private:
   void updateEnvironment(Landscape* environment, unsigned int generation, unsigned int runs);
};

extern "C" {
void callModel(int* x, int* y, int* dispersal, int* runs, double* specRate, bool* dens, 
               bool* env, bool* neutral, bool* mort, int* mortStrength, bool* repro, int* dispersalCutoff, 
               int* densityCutoff, int* seed, std::string* saveLocation, std::string* specOut, 
               std::string* traitOut, std::string* neutralOut, std::string* compOut, std::string* envOut,  std::string* phyloOut);
// void runModel(model* Model, int* runs);
// void getModelData(model* Model, int* dispersal, int* specOut, double* traitOut, double* envOut, std::string* phyloOut);
}

#endif /* PHYLSIMMODEL_H_ */
