/*
 * main.cpp
 *
 *  Created on: 24.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Grid.h>
#include <Model.h>
#include <Utils/RandomGen.h>
#include <Parameters.h>
#include <stdexcept>
#include <string>
#include <iostream>



int main(){
	unsigned int runs = 3000; // number of Generations
	bool neutral = true; // neutral model or not
	bool dd = false; // Density dependent or independent model
	bool env = false; // environmentally dependent or independent model
	int dispersal = 1; // 1 = global dispersal, 2 = nearest neighbor dispersal, 3= kernel dispersal
	int xDim = 1000; // Number of grid cells in x-direction
	int yDim = 1000; // number of grid cells in y-direction
//	srand(1500);
	double specrate = 2.0;
	int densityCut = 1;
	int dispersalCut = 2;
	RandomGen ran;
	ran.seedrand(1500);
	// Just to test
	Parameters* pa = new Parameters();
	std::cout << "Value of numberOfRuns: " << pa->getParameterValue<int>(std::string("numberOfRuns")) << std::endl;

	// avoid inconsistent input
	if(neutral && dd) throw std::runtime_error("A neutral model can't be density dependent!");
	if(neutral && env) throw std::runtime_error("A neutral model can't be depending on the environment!");


	//Running the model
	model Model(xDim,yDim,dispersal, neutral, dd, env, runs, specrate, dispersalCut, densityCut);
	Model.update(runs);
//	Model.get_data();
	Model.getclimate();
//	Model.gettraits();
	if(dispersal ==1)
	{
	Model.m_Global->m_Phylogeny.prunePhylogeny(Model.m_Global->m_Phylogeny.m_FullPhylogeny);
	Model.m_Global->m_Phylogeny.writePhylogeny(1, runs, Model.m_Global->m_Phylogeny.m_PrunedPhylo,'P');
	Model.m_Global->m_Phylogeny.writePhylogeny(1, runs, Model.m_Global->m_Phylogeny.m_FullPhylogeny,'F');
	Model.m_Global->m_Phylogeny.writeSpeciesData();
	}
	else
	{
	Model.m_Local->m_Phylogeny.prunePhylogeny(Model.m_Local->m_Phylogeny.m_FullPhylogeny);
	Model.m_Local->m_Phylogeny.writePhylogeny(1, runs, Model.m_Local->m_Phylogeny.m_PrunedPhylo,'P');
	Model.m_Local->m_Phylogeny.writePhylogeny(1, runs, Model.m_Local->m_Phylogeny.m_FullPhylogeny,'F');
	Model.m_Local->m_Phylogeny.writeSpeciesData();
	}

	return 0;
}


