/*
 * model.h
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 */

#ifndef PHYLSIMMODEL_H_
#define PHYLSIMMODEL_H_

#include "debug.h"

#include <string>

#include "Grid.h"

class PhylSimModel {
 public:
   int m_Dispersal;
   int m_X_coordinate;
   int m_Y_coordinate;
   int timeStep;
   GlobalEnvironment* m_Global;
   LocalEnvironment* m_Local;

   PhylSimModel(int x, int y, int dispersal, int simulationEnd, double specRate, bool dens, 
               bool env, bool neutral, bool mort, int mortStrength, bool repro, int dispersalCutoff, 
               int densityCutoff,std::string saveLocation, double envStrength, double compStrength,
			   int fission, double redQueen, double redQueenStrength);
   ~PhylSimModel();
   void get_data();
   void getclimate();
   void update(unsigned int runs);
   void gettraits();
   void updateGrid();

private:
};


#endif /* PHYLSIMMODEL_H_ */
