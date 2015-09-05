/*
 * individual.h
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef INDIVIDUAL_H_
#define INDIVIDUAL_H_

#include "./RandomGen.h"
//#include "Utils/RandomGen.h"
#include "Species.h"

/**
 * An individual is a realization of a species with individual properties.
 */
class Individual {
 private:
	double kernel(double distance);
	double evolution(double ancestor, double species, double weightSpecies, double weightRandom);

 public:
	Individual();
	Individual(const Individual &ind);
	~Individual();
	void operator=(const Individual &ind);
	// Compute the euclidian_distance from the origin (0, 0) to the given position (ref_x, ref_y)
	double euclidian_distance(int ref_x, int ref_y);
	// The species to which this individual belongs.
	Species * m_Species;

	// The position of this individual.
	int m_X_coordinate;
	int m_Y_coordinate;

	double m_LocalDensity;
	unsigned long long m_Age;
	double m_DensityStrength;
	double m_FitnessWeight;
	int m_Weight;
	double m_Variance;
	double m_Mean;
	double m_CompetitionMarker;
	double m_NeutralMarker;
	double m_dispersalDistance;
  
  double m_envStrength;
  double m_compStrength;

	RandomGen m_RandomGenerator;

	double getSeedsTo(int rel_x, int rel_y, int dispersal_type, double temp, bool env, bool dd);
  
  double getFitness(double temp, bool env, bool dd);

	double dispersal(int dispersal_type, double distance);  // 1 for kernel, 2 for nearest neighbor, 3 for global
};

#endif /* INDIVIDUAL_H_ */
