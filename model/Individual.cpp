/*
 * individual.cpp
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include "Individual.h"

#include <float.h>
#include <cmath>



Individual::Individual()
{
	this ->m_Species = new Species();
	this ->m_X_coordinate = 0;
	this ->m_Y_coordinate = 0;
	this ->m_LocalDensity = 0.0;
	this ->m_Age = 0;
	this ->m_FitnessWeight = 0.5;
	this ->m_DensityStrength = 0.4;
	this ->m_Weight = 1.0;
	this -> m_Variance = 0.04659906;
	this -> m_Mean = 1.0;
	this -> m_CompetitionMarker = 0.5;
	this -> m_NeutralMarker = 0.5;

}

Individual::Individual(const Individual &ind)
{

	this ->m_Species = ind.m_Species;
	this -> m_X_coordinate = ind.m_X_coordinate;
	this -> m_Y_coordinate = ind.m_Y_coordinate;
	this -> m_LocalDensity = ind.m_LocalDensity;
	this -> m_Age = 0;
	this -> m_FitnessWeight = ind.m_FitnessWeight;
	this -> m_DensityStrength = ind.m_DensityStrength;
	this -> m_Weight = ind.m_Weight;
	this -> m_Variance = ind.m_Variance;
	this -> m_Mean = evolution(ind.m_Mean, ind.m_Species->m_Mean, 0.2, 0.1);
	this -> m_CompetitionMarker = evolution(ind.m_CompetitionMarker, ind.m_Species->m_CompetitionMean, 0.2, 0.1);
	this -> m_NeutralMarker = evolution(ind.m_NeutralMarker, ind.m_Species->m_NeutralMean, 0.2, 0.1);

}

Individual::~Individual(){

}


void Individual::operator=(const Individual &ind)
{
	this ->m_Species = ind.m_Species;
	this -> m_X_coordinate = 0;
	this -> m_Y_coordinate = 0;
	this -> m_LocalDensity = ind.m_LocalDensity;
	this -> m_Age = 0;
	this -> m_FitnessWeight = ind.m_FitnessWeight;
	this -> m_DensityStrength = ind.m_DensityStrength;
	this -> m_Weight = ind.m_Weight;
	this -> m_Variance = ind.m_Variance;
	this -> m_Mean = evolution(ind.m_Mean, ind.m_Species->m_Mean, 0.2, 0.1);
	this -> m_CompetitionMarker = evolution(ind.m_CompetitionMarker, ind.m_Species->m_CompetitionMean, 0.2, 0.1);
	this -> m_NeutralMarker = evolution(ind.m_NeutralMarker, ind.m_Species->m_NeutralMean, 0.2, 0.1);

}

	double Individual::kernel(double distance){
		return exp(-distance/2.0);
	}



	double Individual::dispersal(int dispersal_type,double distance)
	{

		if (dispersal_type == 2) //nearest neighbor
		{
			if(distance > 1.0) return 0.0;
			else return 1.0;
		}
		else if (dispersal_type == 3) // kernel
		{
			return kernel(distance);
		}
		else return 0.0;
	}


	double Individual::getSeedsTo(int rel_x, int rel_y, int dispersal_type, double temp, bool env, bool dd)
	{
		double sum_of_weights = 0.0;
		double dispersal_weight = 0.0;

		dispersal_weight = dispersal(dispersal_type, euclidian_distance(rel_x, rel_y)); // Kernel or NN
		if(env)
		{	double envFitness = (1.0 / (m_Variance * sqrt(2.0 * 3.147))) * exp(-0.5 * pow((temp - m_Mean) / m_Variance, 2.0)); // environmental niche
			if(dd)	sum_of_weights = dispersal_weight * envFitness * m_LocalDensity + (DBL_MIN*100.0); //weights plus base value
			else if(!dd)	sum_of_weights = dispersal_weight * envFitness + (DBL_MIN*100.0); //weights plus base value
		}
		else {
			if(dd)	sum_of_weights = dispersal_weight * m_LocalDensity + (DBL_MIN*100.0); //weights plus base value
			else if(!dd)	sum_of_weights = dispersal_weight + (DBL_MIN*100.0); //weights plus base value
		}

		return sum_of_weights;
	}


	double Individual::euclidian_distance(int x, int y)
	{
		return sqrt((x*x) + (y*y));
	}

	double Individual::evolution(double ancestor, double species, double weightSpecies, double weightRandom)
	{
			double max = 0.5;
			double min = -0.5;
			double upperBound = 1.0;
			double lowerBound = 0.0;

			 double randomTrait = m_RandomGenerator.randomDouble(min, max);

		    double newTrait = (1.0 - weightSpecies) * ancestor +  weightSpecies * species + weightRandom * randomTrait;
		    if(newTrait > upperBound) newTrait = upperBound - (newTrait - upperBound);
		    else if(newTrait < lowerBound) newTrait = lowerBound + abs(newTrait);

		return newTrait ;
	}


