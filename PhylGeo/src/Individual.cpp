/*
 * individual.cpp
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa
 *              Florian Hartig
 */

#include "Individual.h"

#include <float.h>
#include <cmath>
#include <stdexcept>



Individual::Individual()
{
  this ->m_Species = new Species();
	this ->m_X_coordinate = 0;
	this ->m_Y_coordinate = 0;
  
	this ->m_LocalDensity = 0.0; // density experienced around itself, will be updated automatically 
	this ->m_Age = 0;
  
  // THESE SEEM OBSOLTE ???
	this ->m_FitnessWeight = 0.5;
	this ->m_DensityStrength = 0.4;
	this ->m_Weight = 1.0;
  // END OBSOLETE
  
	this -> m_Variance = 0.03659906;
	this -> m_Mean = 1.0;
	this -> m_CompetitionMarker = 0.5;
	this -> m_NeutralMarker = 0.5;
  
  this -> m_envStrength = 1;
  this -> m_compStrength = 1;
  
	this -> m_dispersalDistance = 0.0; // parameter for dispersal kernel 

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
	this -> m_Mean = evolution(ind.m_Mean, ind.m_Species->m_Mean, 0.3, 0.05);
	this -> m_CompetitionMarker = evolution(ind.m_CompetitionMarker, ind.m_Species->m_CompetitionMean, 0.3, 0.05);
	this -> m_NeutralMarker = evolution(ind.m_NeutralMarker, ind.m_Species->m_NeutralMean, 0.3, 0.05);
	this -> m_dispersalDistance = ind.m_dispersalDistance;
  
  this -> m_envStrength = ind.m_envStrength;
  this -> m_compStrength = ind.m_compStrength;
}

Individual::~Individual(){

}

// FH - This is a weird construction ... this operator is used 
// for creating a new individual, and applying evolution?
// why not create a child function or something like that
// that would be far more logical
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
	this -> m_Mean = evolution(ind.m_Mean, ind.m_Species->m_Mean, 0.3, 0.05);
	this -> m_CompetitionMarker = evolution(ind.m_CompetitionMarker, ind.m_Species->m_CompetitionMean, 0.3, 0.05);
	this -> m_NeutralMarker = evolution(ind.m_NeutralMarker, ind.m_Species->m_NeutralMean, 0.3, 0.05);
	this -> m_dispersalDistance = ind.m_dispersalDistance;
  
  this -> m_envStrength = ind.m_envStrength;
  this -> m_compStrength = ind.m_compStrength;
}


  // Dispersal Kernel
  // One would think it's easier to calculate this directly in the dispersal
  // Function, but for some reason it seems faster that way

	double Individual::kernel(double distance){
		return exp(-distance/m_dispersalDistance);
	}



	double Individual::dispersal(int dispersal_type, double distance)
	{
		if (dispersal_type == 3) // kernel
		{
      //return exp(-distance / cutoff / 2.0) ; 
			return kernel(distance); // for some weird reason, this option is considerably faster!!!
		}
    else if (dispersal_type == 2) //nearest neighbor
		{
			if(distance > 1.0) return 0.0;
			else return 1.0;
		}
		else throw std::invalid_argument( "Problem in the parameters of dispersal function" );
	}


	double Individual::getSeedsTo(int rel_x, int rel_y, int dispersal_type, double temp, bool env, bool dd)
	{
    double dispersal_weight = 0.0;
		dispersal_weight = dispersal(dispersal_type, euclidian_distance(rel_x, rel_y)); // Kernel or NN
    
		if(env && dd) {
      double fitness_weight = 0.0;
      fitness_weight = getFitness(temp, env, dd);
      return(dispersal_weight * fitness_weight + (DBL_MIN*100.0));
		}else{
      return(dispersal_weight + (DBL_MIN*100.0));
		}
	}
  
   /**
   * gets the fitness of the current individual 
   * @param temp environmental parameter
   * @param env environment acting
   * @param dd density acting 
   * @return Fitness
   */
  double Individual::getFitness(double temp, bool env, bool dd)
	{
		if(env)
		{	
      double envFitness = (m_envStrength * 1.2 * exp(-0.5 * pow((temp - m_Mean) / m_Variance, 2.0)) + 1-m_envStrength); // environmental niche
			
      if(dd)	return envFitness * (m_compStrength * m_LocalDensity + 1- m_compStrength) + (DBL_MIN*100.0); //weights plus base value
			else return	envFitness + (DBL_MIN*100.0); //weights plus base value
		}
		else {
			return (m_compStrength * m_LocalDensity + 1- m_compStrength) + (DBL_MIN*100.0); //weights plus base value
		}
		throw std::invalid_argument( "neither dd nor env true" );
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
		    else if(newTrait < lowerBound) newTrait = lowerBound + std::abs(newTrait);

		return newTrait ;
	}
  
  void Individual::die(int generation){
    
      if(m_Species->m_Count-1 < 1)
      {
         m_Species->m_Date_of_Extinction = generation;
         m_Species->m_Count -= 1;
      }
      else
      {
         m_Species->m_Count -=1;
         m_Species->decMean(m_Mean, m_CompetitionMarker, m_NeutralMarker);
         m_Species->updateMean();
      }
    
  }


