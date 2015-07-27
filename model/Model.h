/*
 * model.h
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 */

#ifndef MODEL_H_
#define MODEL_H_


#include <string>

#include "Grid.h"

class model {
 public:
   int m_Dispersal;
   int m_X_coordinate;
   int m_Y_coordinate;
   GlobalEnvironment* m_Global;
   LocalEnvironment* m_Local;

   model(int x, int y, int type, bool neutral, bool dd,bool env, bool mort, bool repro,
            unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff, unsigned int mortalityStrength, std::string location);
   ~model();
   void get_data();
   void getclimate();
   void update(unsigned int runs, std::string location);
   void gettraits();

private:
   void updateEnvironment(Landscape* environment, unsigned int generation, unsigned int runs);
};

extern "C" {
void callModel(int* x, int* y, int* dispersal, int* runs, double* specRate,
			   bool* dens, bool* env, bool* neutral, bool* mort, unsigned int* mortStrength, bool* repro,
			   int* dispersalCutoff, int* densityCutoff, int* seed, char** saveLocation,
			   int* specOut, double* traitOut, double* neutralOut, double* compOut, double* envOut, char** phyloOut);
// void runModel(model* Model, int* runs);
// void getModelData(model* Model, int* dispersal, int* specOut, double* traitOut, double* envOut, std::string* phyloOut);
}

#endif /* MODEL_H_ */
