/*
 * grid.h
 *
 *  Created on: 20.06.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef GRID_H_
#define GRID_H_

#include <utility>  // needed for std::pair
#include <vector>

#include "Phylogeny.h"
#include "RandomGen.h"
#include "Individual.h"

/**
 * The landscape is the main component of our world. It is separated into global environment and
 * local environment. When the species are globally independently (their reproduction doesn't depend
 * on the other species) distributed then we choose the global environment.
 * In the other case, when the distribution of the species is dependent to the local environmental
 * conditions, then we choose a local environment.
 */
class Landscape {
public:
    int m_Cutoff;
    int m_DensityCutoff;
    int m_Dispersal_type;
    // The size of the landscape.
    int m_Xdimensions, m_Ydimensions;
    // Counter for all the species that are born.
    unsigned long long m_Global_Species_Counter;
    unsigned int m_mortalityStrength;

    // Indicates whether the species dispersion depends on local conditions or not.
    bool m_Neutral;
    bool m_DD;
    bool m_Env;
    // switches to determine wheather fitness should affect mortality or reproduction
    bool m_mortality;
    bool m_reproduction;

    // The size of the landscape is defined as m_Xdimensions * m_Ydimensions
    unsigned int m_LandscapeSize;
    int m_KernelSize;

    double m_AirTemperature;
    double m_SoilMoistureRange;
    double m_GradientStep;
    double m_Speciation_Rate;

    double cellsWithinDensityCutoff;

    double m_envStrength;
    double m_compStrength;
    int m_fission;
    double m_redQueen;
    double m_redQueenStrength;
    int m_protracted;
    std::vector<double> airmat;
    std::vector<double> soilmat;


    // Change the temperature in the environment by the given magnitude.
    void tempChange(int sign, double magnitude);

    // Change the moisture  in the environment by the given magnitude.
    void moistChange(int sign, double magnitude);

    Landscape();

    Landscape(int xsize, int ysize, int type, bool neutral,
              bool dd, bool env, bool mort, bool repro, unsigned int simulationEnd, double specRate,
              int dispersalCutoff, int densityCutoff, unsigned int mortalityStrength, double envStrength,
              double compStrength, int fission, double redQueen, double redQueenStrength, int protracted,
              std::vector<double> airmat, std::vector<double> soilmat);

    virtual ~Landscape();

    // TODO(Betim): Should it really be public?
    Phylogeny m_Phylogeny;

    // A 2-D array of individuals.
    Individual **m_Individuals;
    std::vector <std::pair<double, double> > m_Environment;
    RandomGen m_RandomGenerator;

    unsigned int m_SimulationEnd;

    // Start the reproduction of the species.
    virtual void reproduce(unsigned int generation);

    // Increase the age of each individual by 1.
    void increaseAge(unsigned int generation);

    void speciation(unsigned int generation);

    void densityUpdate(int x, int y);


    std::pair<int, int> get_dimensions();
};

class GlobalEnvironment : public Landscape {
public:
    GlobalEnvironment();

    GlobalEnvironment(int xsize, int ysize, int type, bool neutral, bool dd, bool env, bool mort, bool repro,
                      unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff,
                      unsigned int mortalityStrength,
                      double envStrength, double compStrength, int fission, double redQueen, double redQueenStrength,
                      int protracted, std::vector<double> airmat, std::vector<double> soilmat);

    virtual ~GlobalEnvironment();

    void reproduce(unsigned int generation);
};

class LocalEnvironment : public Landscape {
public:
    LocalEnvironment();

    LocalEnvironment(int xsize, int ysize, int type, bool neutral, bool dd, bool env, bool mort, bool repro,
                     unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff,
                     unsigned int mortalityStrength,
                     double envStrength, double compStrength, int fission, double redQueen, double redQueenStrength,
                     int protracted, std::vector<double> airmat, std::vector<double> soilmat);

    virtual ~LocalEnvironment();

    void reproduce(unsigned int generation);
};

#endif /* GRID_H_ */

