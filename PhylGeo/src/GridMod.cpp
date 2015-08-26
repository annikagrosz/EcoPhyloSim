/*
 * gridMod.cpp
 *
 *  Created on: 12.01.2015
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */


#include <cfloat>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <utility>
#include <vector>

#include "Grid.h"
#include "Individual.h"
#include "Species.h"
//#include "./Utils/RandomGen.h"
#include "./RandomGen.h"


//Landscape::Landscape()
//{
//  m_Cutoff= 4;
//	m_Neutral = false;
//	m_DD = true;
//	m_Env = true;
//	m_DensityCutoff = 2;
//	m_Runs= 100;
//	m_Dispersal_type = 3;
//	m_Global_Species_Counter = 1;
//	m_Speciation_Rate = 2.0;
//	m_Xdimensions = 256;
//	m_Ydimensions = 256;
//	m_LandscapeSize = m_Xdimensions * m_Ydimensions;
//	m_RandomGenerator.seedrand(1500);
//
//
//
//
//	this->m_Individuals = new Individual * [m_Xdimensions];
//	for(int cols = 0; cols < m_Xdimensions;cols++)
//	{
//	   m_Individuals[cols] = new Individual [m_Ydimensions];
//	}
//
//	initialize(m_Xdimensions,m_Ydimensions, m_Runs);
//
//	m_AirTemperature =  0.0; // Celsius
//	m_GradientStep = 0.0078125;
//	m_SoilMoistureRange = 101.0; // percent
//
//	for(int i = 0; i < m_Xdimensions; i++)
//	{
//		for(int j = 0; j < m_Ydimensions; j++)
//		{
//			doubleCell envi;
//			envi.first = m_AirTemperature;
//			envi.second = 1.0;
//			m_Environment.push_back(envi);
//		}
//		if(i < m_Xdimensions / 2) m_AirTemperature += m_GradientStep;
//		else m_AirTemperature -= m_GradientStep;
//	}
//}


Landscape::Landscape(int xsize, int ysize, int type, bool neutral, bool dd, bool env, bool mort, bool repro, unsigned int runs, double specRate, int dispersalCutoff, int DensityCutoff, unsigned int mortalityStrength)
{

   m_Cutoff= dispersalCutoff;
   m_Neutral = neutral;
   m_DD = dd;
   m_Env = env;
   m_mortality = mort;
   m_reproduction = repro;
   m_Runs = runs;
   m_DensityCutoff = DensityCutoff;
   m_Dispersal_type = type;
   m_Global_Species_Counter = 1;
   m_Speciation_Rate = specRate;
   m_LandscapeSize = xsize * ysize;
   m_Xdimensions = xsize;
   m_Ydimensions = ysize;
   m_mortalityStrength = mortalityStrength;
   //	func.seedrand(1500);

   // Construct grid of individuals
   this ->m_Individuals = new Individual * [m_Xdimensions];
   for(int cols = 0; cols < m_Xdimensions;cols++)
   {
      m_Individuals[cols] = new Individual[m_Ydimensions];
   }

   // Initialization
   initialize(m_Xdimensions,m_Ydimensions, m_Runs);

   m_AirTemperature =  0.0; // Celsius
   m_GradientStep = (1.0/(double)m_Xdimensions)*2.0;
   m_SoilMoistureRange = 101.0; // percent

   for(int i = 0; i < m_Xdimensions; i++)
   {
      for(int j = 0; j < m_Ydimensions; j++)
      {
         std::pair<double, double> envi;
         envi.first = m_AirTemperature;
         envi.second = 1.0;
         m_Environment.push_back(envi);
      }
      if(i < m_Xdimensions / 2) m_AirTemperature += m_GradientStep;
      else m_AirTemperature -= m_GradientStep;
   }
}

Landscape::~Landscape()
{
   for(int i = 0; i < m_Xdimensions; i++)
   {
      delete [] m_Individuals[i];
   }
   delete[] m_Individuals;
}

void Landscape::reproduce(unsigned int generation) {
   // (Betim): This method is needed in order to use polymorphism method calls.
   // (e.g. Model->updateEnvironment, the argument Landscape needs the method "reproduce"
   // to be defined, although it is void.)
}

std::pair<int, int> Landscape::get_dimensions()
{
   std::pair<int,int>(dimensions);
   dimensions = std::make_pair(m_Xdimensions, m_Ydimensions);
   return dimensions;
}

void Landscape::initialize(int xsize, int ysize,unsigned int runs)
{
   Species * spec = new Species(1, 1, 0, std::make_pair<int,int>(0,0), runs);

   for(int cols = 0; cols < xsize;cols++)
   {
      for(int rows=0; rows < ysize; rows++)
      {
         this ->m_Individuals[cols][rows].m_Species = spec;
         this ->m_Individuals[cols][rows].m_X_coordinate = cols;
         this->m_Individuals[cols][rows].m_Y_coordinate =rows;
         this->m_Individuals[cols][rows].m_Species->m_Count += 1;
         this->m_Individuals[cols][rows].m_Species->sumMean(m_Individuals[cols][rows].m_Mean, m_Individuals[cols][rows].m_CompetitionMarker, m_Individuals[cols][rows].m_NeutralMarker);
         this->m_Individuals[cols][rows].m_Species->updateMean();
         //				this->individuals[cols][rows].Species->date_of_extinction = runs;
      }
   }
   m_Phylogeny.updatePhylogeny(m_Individuals[xsize-1][ysize-1].m_Species);
}

void Landscape::increaseAge()
{

   for (int rows = 0; rows < this->m_Xdimensions; rows++)
   {
      for (int cols =0; cols < this->m_Ydimensions; cols++)
      {
         this->m_Individuals[rows][cols].m_Age += 1;
      }
   }
}

void Landscape::tempChange(int sign, double magnitude)
{
   for(int i = 0; i < m_Xdimensions; i++)
   {
      for(int j = 0; j < m_Ydimensions; j++)
      {
         m_Environment[i * m_Ydimensions + j].first = m_Environment[i * m_Ydimensions + j].first * sign * magnitude;
      }
   }
}

void Landscape::moistChange(int sign, double magnitude)
{
   for(int i = 0; i < m_Xdimensions; i++)
   {
      for(int j = 0; j < m_Ydimensions; j++)
      {
         m_Environment[i * m_Ydimensions + j].second = m_Environment[i * m_Ydimensions + j].second * sign * magnitude;
      }
   }
}


GlobalEnvironment::GlobalEnvironment(int xsize, int ysize, int type, bool neutral, bool dd, bool env, bool mort, bool repro, unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff, unsigned int mortalityStrength) :
	                              Landscape(xsize,  ysize,  type,  neutral,  dd,  env, mort, repro,  runs, specRate, dispersalCutoff, densityCutoff, mortalityStrength)
{

}

GlobalEnvironment::~GlobalEnvironment() {
   // TODO: Implement destructor!
}

void GlobalEnvironment::reproduce(unsigned int generation)
{
   if(m_Neutral)
   {
      int x_coordinate, y_coordinate, x_parent, y_parent;

      //			srand(time(0));
      for(unsigned int event = 0; event < m_LandscapeSize; event++)
      {

         x_coordinate =  m_RandomGenerator.randomInt(0,m_Xdimensions-1); // rand() % xdimensions;
         y_coordinate = m_RandomGenerator.randomInt(0,m_Ydimensions-1); // rand() % ydimensions;

         x_parent = m_RandomGenerator.randomInt(0,m_Xdimensions-1); //rand() % xdimensions;
         y_parent = m_RandomGenerator.randomInt(0,m_Ydimensions-1); //rand() % ydimensions;
         //				std::cout << x_coordinate << "::" << x_parent << "::" << y_coordinate << "::" << y_parent << '\n';
         if(m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count-1 < 1)
         {
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Date_of_Extinction = generation;
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count -= 1;
         }

         else
         {
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count-=1;
            m_Individuals[x_coordinate][y_coordinate].m_Species->decMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
            m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();
         }

         m_Individuals[x_coordinate][y_coordinate] = m_Individuals[x_parent][y_parent]; //deep copy !
         //				individuals[x_coordinate][y_coordinate].x_coordinate = x_coordinate;
         //				individuals[x_coordinate][y_coordinate].y_coordinate = y_coordinate;
         m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count += 1;
         m_Individuals[x_coordinate][y_coordinate].m_Species->sumMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
         m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();
         //				double survival = (rand() % 101) / 100.0;
      }
   }

   else if(!(m_Neutral)) // Density dependence and / or Environmental dependence
   {
      int x_parent = 0;
      int y_parent = 0;
      unsigned int array_length = 0;
      unsigned int numberOfRuns = 0;
      double seedSum = 0.0;
      int new_parent = 0;

      m_KernelSize =(m_Xdimensions)*(m_Ydimensions);

      double weights[m_KernelSize];
      double cumWeights[m_KernelSize] ;
      std::vector<std::pair<int, int> > parents (m_KernelSize);


      for(int kernel_x = 0; kernel_x < m_Xdimensions; kernel_x++)
      {
         for(int kernel_y = 0 ; kernel_y <  m_Ydimensions ; kernel_y++)
         {
            if(m_Env)
            {
               double envFitnessParent = 1.2 * exp(-0.5 * pow((m_Environment[kernel_x * m_Ydimensions + kernel_y].first - m_Individuals[kernel_x][kernel_y].m_Mean) / m_Individuals[kernel_x][kernel_y].m_Variance, 2.0));
               if(m_DD) weights[array_length] = envFitnessParent  * m_Individuals[kernel_x][kernel_y].m_LocalDensity + (DBL_MIN*100.0); //weights plus base value
               else if (!m_DD) weights[array_length] = envFitnessParent  + (DBL_MIN*100.0) ; //weights plus base value
               //								std::cout <<  weights[array_length] << '\n';
            }
            else if (!m_Env)
            {
               weights[array_length] = m_Individuals[kernel_x][kernel_y].m_LocalDensity + (DBL_MIN*100.0); //weights plus base value
            }
            parents[array_length].first = kernel_x;
            parents[array_length].second = kernel_y;
            seedSum += weights[array_length];
            array_length ++;
         }
      }
      //			std::cout << array_length << " : " << kernelsize <<   '\n';
      cumWeights[0] = weights[0] ;

      for(unsigned int i =1; i< array_length; i++)
      {
         cumWeights[i] = weights[i] + cumWeights[i-1];
         //				std::cout << seedSum << " : " << cumWeights[i] << " : " << weights[i] << " : " << i <<  '\n';
         //				std::cout << cumWeights[i] << '\n';
      }
      //			std::cout << seedSum << " : " << cumWeights[kernelsize-1] << " : " << weights[kernelsize-1]<< " : " << kernelsize-1 <<  '\n';

      if(m_mortality) numberOfRuns = m_LandscapeSize*2;
      else numberOfRuns = m_LandscapeSize;

      for(unsigned int event = 0; event < numberOfRuns; event++)
      {

         int x_coordinate = m_RandomGenerator.randomInt(0,m_Xdimensions-1);
         int y_coordinate = m_RandomGenerator.randomInt(0,m_Ydimensions-1);

         if(event % m_mortalityStrength != 0 && m_mortality){// important!! the frequency in relation to the base mortality controls the intensity of the mechanisms
        	 	 double chanceOfDeath = m_RandomGenerator.randomDouble(0.0,1.0);
           		  if(weights[x_coordinate * m_Ydimensions + y_coordinate] > chanceOfDeath){
           			  continue;
           		  }
           	  }


         if(m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count-1 < 1)
         {
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Date_of_Extinction = generation;
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count -= 1;
         }
         else
         {
            m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count -=1;
            m_Individuals[x_coordinate][y_coordinate].m_Species->decMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
            m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();
         }

         if(m_reproduction) new_parent = m_RandomGenerator.multinomialDraw(cumWeights, array_length-1, seedSum);
         else if (!m_reproduction && m_mortality) new_parent = m_RandomGenerator.randomInt(0, array_length-1);
         x_parent = parents[new_parent].first;
         y_parent = parents[new_parent].second;

         m_Individuals[x_coordinate][y_coordinate] = m_Individuals[x_parent][y_parent]; //deep copy !
         m_Individuals[x_coordinate][y_coordinate].m_X_coordinate = x_coordinate;
         m_Individuals[x_coordinate][y_coordinate].m_Y_coordinate = y_coordinate;
         m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count += 1;
         m_Individuals[x_coordinate][y_coordinate].m_Species->sumMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
         m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();
         parents[x_coordinate*m_Ydimensions + y_coordinate].first = x_coordinate;
         parents[x_coordinate*m_Ydimensions + y_coordinate].second = y_coordinate;


         if(!(m_DD))
         {
            double newWeight = 0.0;
            if(m_Env)
            {
               double envFitnessParent = 1.2 * exp(-0.5 * pow((m_Environment[x_coordinate * m_Ydimensions + y_coordinate].first - m_Individuals[x_coordinate][y_coordinate].m_Mean) / m_Individuals[x_coordinate][y_coordinate].m_Variance, 2.0));
               //						double envFitnessPropagule = (1.0 / (individuals[kernel_x][kernel_y].variance * sqrt(2.0 * 3.147))) * exp(-0.5 * pow((environment[x_coordinate][y_coordinate].first - individuals[kernel_x][kernel_y].mean) / individuals[kernel_x][kernel_y].variance, 2.0)); // environmental influence !
               newWeight = envFitnessParent  + (DBL_MIN*100.0) ; //weights plus base value
            }

            //						double envFitnessParent = (1.0 / (individuals[x_coordinate][y_coordinate].variance * sqrt(2.0 * 3.147))) * exp(-0.5 * pow((environment[x_coordinate][y_coordinate].first - individuals[x_coordinate][y_coordinate].mean) / individuals[x_coordinate][y_coordinate].variance, 2.0)); // environmental influence !
            //						double envFitnessPropagule = (1.0 / (individuals[x_coordinate][y_coordinate].variance * sqrt(2.0 * 3.147))) * exp(-0.5 * pow((environment[x_coordinate][y_coordinate].first - individuals[x_coordinate][y_coordinate].mean) / individuals[x_coordinate][y_coordinate].variance, 2.0)); // environmental influence !
            //						newWeight = envFitnessParent * envFitnessPropagule + (DBL_MIN*100.0);
            else throw std::runtime_error("This should not happen, Neutral conditions are defined differently");

            unsigned int vecPos = x_coordinate*m_Ydimensions + y_coordinate;
            double oldWeight = weights[vecPos];
            weights[vecPos] = newWeight;

            double diff = newWeight - oldWeight; // recalculate accumulated weights
            seedSum += diff;
            if (oldWeight < 0.)std::cout << oldWeight << '\n';
            if (newWeight < 0.)std::cout << newWeight << '\n';

            if(vecPos == 0)
            {
               cumWeights[0] = weights[0];
            }
            else
            {
               cumWeights[vecPos] = cumWeights[vecPos -1] + newWeight;
            }

            for (unsigned int i = vecPos+1; i < array_length; i++)
            {
               cumWeights[i] += diff;
               //						std::cout << cumWeights[i] << '\n';
            }
            cumWeights[0] = weights[0] ;
            for(unsigned int i =1; i< array_length; i++)
            {
               cumWeights[i] = weights[i] + cumWeights[i-1];
            }
         }

         else if(m_DD)
         {
            int  densityKernel_x, densityKernel_y, focus_x, focus_y;
            double relatedness = 0.0;
            double cells = double((m_DensityCutoff * 2 +1) * (m_DensityCutoff * 2 +1) - 1) ;

            for(int relativeX= - m_DensityCutoff; relativeX <= m_DensityCutoff; relativeX++)
            {
               for(int relativeY = - m_DensityCutoff ; relativeY <=  m_DensityCutoff ; relativeY++)
               {
                  // get focus cell
                  focus_x = ((x_coordinate + relativeX + m_Xdimensions) % m_Xdimensions);
                  focus_y = ((y_coordinate + relativeY + m_Ydimensions) % m_Ydimensions);

                  for(int relativeX2= - m_DensityCutoff; relativeX2 <= m_DensityCutoff; relativeX2++)
                  {
                     for(int relativeY2 = - m_DensityCutoff ; relativeY2 <=  m_DensityCutoff ; relativeY2++)
                     {
                        densityKernel_x = ((focus_x + relativeX2 + m_Xdimensions) % m_Xdimensions);
                        densityKernel_y = ((focus_y + relativeY2 + m_Ydimensions) % m_Ydimensions);

                        if (!(densityKernel_x == focus_x && densityKernel_y == focus_y))
                        {
                           relatedness += std::abs(m_Individuals[focus_x][focus_y].m_CompetitionMarker - m_Individuals[densityKernel_x][densityKernel_y].m_CompetitionMarker);
                        }
                     }
                  }
                  m_Individuals[focus_x][focus_y].m_LocalDensity = relatedness / cells;

                  if(m_Env)
                  {
                     double envFitnessParent = 1.2 * exp((-0.5 * (m_Environment[focus_x * m_Ydimensions + focus_y].first - m_Individuals[focus_x][focus_y].m_Mean) / m_Individuals[focus_x][focus_y].m_Variance * (m_Environment[focus_x * m_Ydimensions + focus_y].first - m_Individuals[focus_x][focus_y].m_Mean) / m_Individuals[focus_x][focus_y].m_Variance)); // environmental influence !
                     //								double envFitnessPropagule = (1.0 / (individuals[focus_x][focus_y].variance * sqrt(2.0 * 3.147))) * exp((-0.5 * (environment[x_coordinate][y_coordinate].first - individuals[focus_x][focus_y].mean) / individuals[focus_x][focus_y].variance * (environment[x_coordinate][y_coordinate].first - individuals[focus_x][focus_y].mean) / individuals[focus_x][focus_y].variance)); // environmental influence !

                     seedSum -= weights[focus_x*m_Ydimensions + focus_y ];
                     weights[focus_x*m_Ydimensions + focus_y ] = envFitnessParent  * m_Individuals[focus_x][focus_y].m_LocalDensity + (DBL_MIN*100.0);
                     seedSum += weights[focus_x*m_Ydimensions + focus_y ];
                  }
                  else
                  {
                     seedSum -= weights[focus_x*m_Ydimensions + focus_y ];
                     weights[focus_x*m_Ydimensions + focus_y ] = m_Individuals[focus_x][focus_y].m_LocalDensity + (DBL_MIN*100.0);
                     seedSum +=  weights[focus_x*m_Ydimensions + focus_y ];
                  }
               }
            }

            unsigned int start =  ((x_coordinate -m_DensityCutoff + m_Xdimensions) % m_Xdimensions) * m_Ydimensions + ((y_coordinate - m_DensityCutoff + m_Ydimensions) % m_Ydimensions) ;
            unsigned int end = array_length;

            if(start == 0) cumWeights[0] = weights[0] ;
            else cumWeights[start] = cumWeights[start-1] + weights[start] ;

            for(unsigned int kk = start+1; kk < end; kk++)
            {
               cumWeights[kk] = weights[kk] + cumWeights[kk-1];
               //						std::cout << seedSum << " : " << cumWeights[kk] << " : " << kk << " : " << array_length-1 <<  '\n';
            }
            //					std::cout << seedSum << " : " << cumWeights[array_length-1] << " : " << array_length-1  << " : " << start  << " : " << end <<  '\n';
         }

         //				std::cout << seedSum << " : " << cumWeights[array_length-1] << " : " << array_length-1 <<  '\n';

      }
   }
}


LocalEnvironment::LocalEnvironment(int xsize, int ysize, int type, bool neutral, bool dd, bool env,bool mort, bool repro, unsigned int runs, double specRate, int dispersalCutoff, int densityCutoff, unsigned int mortalityStrength) :
	                              Landscape(xsize,  ysize,  type,  neutral,  dd,  env, mort, repro,  runs, specRate, dispersalCutoff, densityCutoff, mortalityStrength)
{

}

LocalEnvironment::~LocalEnvironment() {
   // TODO: Implement destructor!
}

void LocalEnvironment::reproduce(unsigned int generation)
{
   int x_coordinate;
   int y_coordinate;
   int x_parent = 0, kernel_x = 0;
   int y_parent = 0, kernel_y = 0;
   unsigned int array_length;
   double seedSum = 0.0;
   int new_parent;
   unsigned int numberOfRuns = 0;
   m_KernelSize = ((2*m_Cutoff+1)*(2*m_Cutoff+1)-1);

   if(m_mortality) numberOfRuns =  m_LandscapeSize*2;
   else numberOfRuns =  m_LandscapeSize;
   
   
   double densityCells = 0 ;

   for(int relativeX2= - m_DensityCutoff; relativeX2 <= m_DensityCutoff; relativeX2++)
   {
      int yLims2 = floor(sqrt(m_DensityCutoff* m_DensityCutoff - relativeX2*relativeX2 )); // avoid diagonal bias
      for(int relativeY2 = - yLims2 ; relativeY2 <=  yLims2 ; relativeY2++)
      {
        densityCells += 1.0 ;
      }
   } 
  

   for(unsigned int event = 0; event < numberOfRuns; event++)
   {

      x_coordinate = m_RandomGenerator.randomInt(0,m_Xdimensions-1);
      y_coordinate = m_RandomGenerator.randomInt(0,m_Ydimensions-1);

      if(event % m_mortalityStrength != 0 && m_mortality){ // important!! the frequency in relation to the base mortality controls the intensity of the mechanisms
    	  double weight = m_Individuals[kernel_x][kernel_y].getSeedsTo(0,0, m_Dispersal_type, m_Environment[x_coordinate * m_Ydimensions + y_coordinate].first, m_Env, m_DD, m_Cutoff);
    	  //std::cout<<weight<<"\n"; write out weights
        double chanceOfDeath = m_RandomGenerator.randomDouble(0.0,1.0);
		  if(weight > chanceOfDeath){
			  continue;
		  }
	  }

      if(m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count-1 < 1)
      {
         m_Individuals[x_coordinate][y_coordinate].m_Species->m_Date_of_Extinction = generation;
         m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count -= 1;
      }
      else
      {
         m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count -=1;
         m_Individuals[x_coordinate][y_coordinate].m_Species->decMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
         m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();
      }


      std::vector<std::pair<int, int> > parents (m_KernelSize);
      double weights[m_KernelSize];
      array_length = 0;
      seedSum = 0.0;
      
      // DISPERSAL 
      // TODO ... implement round cutoff directly through the coordinates !!!
      
      for(int relativeX = - m_Cutoff; relativeX <= m_Cutoff; relativeX++)
      {
         int yLims = floor(sqrt(m_Cutoff* m_Cutoff - relativeX*relativeX )); // avoid diagonal bias
         for(int relativeY = - yLims ; relativeY <=  yLims ; relativeY++)
         {
            kernel_x = (x_coordinate + relativeX + m_Xdimensions) % m_Xdimensions;
            kernel_y = (y_coordinate + relativeY + m_Ydimensions) % m_Ydimensions;
            if (!(kernel_x == x_coordinate && kernel_y == y_coordinate)) 
            {
               parents[array_length].first = kernel_x;
               parents[array_length].second =  kernel_y;
               if(m_reproduction) weights[array_length] = m_Individuals[kernel_x][kernel_y].getSeedsTo(relativeX,relativeY, m_Dispersal_type, m_Environment[kernel_x * m_Ydimensions + kernel_y].first, m_Env, m_DD, m_Cutoff);
               else if(!m_reproduction && m_mortality)  weights[array_length] = m_Individuals[kernel_x][kernel_y].dispersal(m_Dispersal_type, m_Individuals[kernel_x][kernel_y].euclidian_distance(relativeX, relativeY), m_Cutoff);
               seedSum += weights[array_length];
               array_length +=1;
            }
         }
      }

      double cumWeights[array_length] ;
      cumWeights[0] = weights[0];
      for(unsigned int i =1; i< array_length; i++)
      {
         cumWeights[i] = weights[i] + cumWeights[i-1];
         //				std::cout << cumWeights[i] << '\n';
      }

      new_parent = m_RandomGenerator.multinomialDraw(cumWeights, array_length-1, seedSum);
      x_parent = parents[new_parent].first;
      y_parent = parents[new_parent].second;

      m_Individuals[x_coordinate][y_coordinate] = m_Individuals[x_parent][y_parent]; //deep copy !
      m_Individuals[x_coordinate][y_coordinate].m_X_coordinate = x_coordinate;
      m_Individuals[x_coordinate][y_coordinate].m_Y_coordinate = y_coordinate;
      m_Individuals[x_coordinate][y_coordinate].m_Species->m_Count += 1;
      m_Individuals[x_coordinate][y_coordinate].m_Species->sumMean(m_Individuals[x_coordinate][y_coordinate].m_Mean, m_Individuals[x_coordinate][y_coordinate].m_CompetitionMarker, m_Individuals[x_coordinate][y_coordinate].m_NeutralMarker);
      m_Individuals[x_coordinate][y_coordinate].m_Species->updateMean();


      if(m_DD)
      {
         int focus_x, focus_y, densityKernel_x, densityKernel_y;
         double relatedness;

         for(int relativeX= - m_DensityCutoff; relativeX <= m_DensityCutoff; relativeX++)
         {
            int yLims = floor(sqrt(m_DensityCutoff* m_DensityCutoff - relativeX*relativeX )); // avoid diagonal bias
            for(int relativeY = - yLims ; relativeY <=  yLims ; relativeY++)
            {
               // get focus cell
               focus_x = ((x_coordinate + relativeX + m_Xdimensions) % m_Xdimensions);
               focus_y = ((y_coordinate + relativeY + m_Ydimensions) % m_Ydimensions);

               relatedness = 0.0;

               for(int relativeX2= - m_DensityCutoff; relativeX2 <= m_DensityCutoff; relativeX2++)
               {
                  int yLims2 = floor(sqrt(m_DensityCutoff* m_DensityCutoff - relativeX2*relativeX2 )); // avoid diagonal bias
                  for(int relativeY2 = - yLims2 ; relativeY2 <=  yLims2 ; relativeY2++)
                  {
                     densityKernel_x = ((focus_x + relativeX2 + m_Xdimensions) % m_Xdimensions);
                     densityKernel_y = ((focus_y + relativeY2 + m_Ydimensions) % m_Ydimensions);

                     if (!(densityKernel_x == focus_x && densityKernel_y == focus_y))
                     {
                        relatedness += std::abs(m_Individuals[focus_x][focus_y].m_CompetitionMarker - m_Individuals[densityKernel_x][densityKernel_y].m_CompetitionMarker);
                     }
                  }
               }
               m_Individuals[focus_x][focus_y].m_LocalDensity = relatedness / densityCells;
            }
         }
      }
   }
}



void Landscape::speciation (unsigned int generation,unsigned int runs)
{
   // std::cout << generation << '\n';
   std::pair<int, int> birthplace;
   int specRate = m_RandomGenerator.randomPoisson(m_Speciation_Rate);

   for (int i = 0; i < specRate; i++)
   {
      int x = m_RandomGenerator.randomInt(0,m_Xdimensions-1); // rand() % xdimensions;
      int y = m_RandomGenerator.randomInt(0,m_Ydimensions-1); // rand() % ydimensions;

      m_Global_Species_Counter+=1;

      birthplace.first = x;
      birthplace.second = y;

      double max = 1.0;
      double min = 0.0;
      double upper = 1.0;
      double lower = -1.0;

      double oldMean = m_Individuals[x][y].m_Mean;
      double oldNeutralMarker = m_Individuals[x][y].m_NeutralMarker;
      double oldCompetitionMarker = m_Individuals[x][y].m_CompetitionMarker;

      double newMean = m_RandomGenerator.randomDouble(lower,upper);
      double newCompetitionMarker = m_RandomGenerator.randomDouble(lower,upper);
      double newNeutralMarker = m_RandomGenerator.randomDouble(lower,upper);

      m_Individuals[x][y].m_Species->m_Children.push_back(m_Global_Species_Counter);

      if(m_Individuals[x][y].m_Species->m_Count-1 < 1)
      {
         m_Individuals[x][y].m_Species->m_Date_of_Extinction = generation;
         m_Individuals[x][y].m_Species->m_Count -= 1;
      }
      else
      {
         m_Individuals[x][y].m_Species->m_Count -= 1;
         m_Individuals[x][y].m_Species->decMean(m_Individuals[x][y].m_Mean, m_Individuals[x][y].m_CompetitionMarker, m_Individuals[x][y].m_NeutralMarker);
         m_Individuals[x][y].m_Species->updateMean();
      }


      m_Individuals[x][y].m_Species = new Species(m_Global_Species_Counter, m_Individuals[x][y].m_Species->get_species_ID(), generation, std::make_pair(birthplace.first, birthplace.second), m_Runs);
      m_Individuals[x][y].m_Age = 0;

      m_Individuals[x][y].m_Mean =  oldMean + (0.2 * newMean);
      if(m_Individuals[x][y].m_Mean > max) m_Individuals[x][y].m_Mean = m_Individuals[x][y].m_Mean - max;
      else if(m_Individuals[x][y].m_Mean < min) m_Individuals[x][y].m_Mean = min + std::abs(m_Individuals[x][y].m_Mean);

      m_Individuals[x][y].m_NeutralMarker =   oldNeutralMarker + (0.2 * newNeutralMarker);
      if(m_Individuals[x][y].m_NeutralMarker > max) m_Individuals[x][y].m_NeutralMarker = m_Individuals[x][y].m_NeutralMarker - max;
      else if(m_Individuals[x][y].m_NeutralMarker < min) m_Individuals[x][y].m_NeutralMarker = min + std::abs(m_Individuals[x][y].m_NeutralMarker);

      m_Individuals[x][y].m_CompetitionMarker =   oldCompetitionMarker + (0.2 * newCompetitionMarker);
      if(m_Individuals[x][y].m_CompetitionMarker > max) m_Individuals[x][y].m_CompetitionMarker = m_Individuals[x][y].m_CompetitionMarker - max;
      else if(m_Individuals[x][y].m_CompetitionMarker < min) m_Individuals[x][y].m_CompetitionMarker = min + std::abs(m_Individuals[x][y].m_CompetitionMarker);

      m_Individuals[x][y].m_Species->m_Mean = m_Individuals[x][y].m_Mean;
      m_Individuals[x][y].m_Species->m_CompetitionMean = m_Individuals[x][y].m_CompetitionMarker;
      m_Individuals[x][y].m_Species->m_NeutralMean = m_Individuals[x][y].m_NeutralMarker;

      m_Individuals[x][y].m_Species->m_Count += 1;
      m_Individuals[x][y].m_Species->sumMean(m_Individuals[x][y].m_Mean, m_Individuals[x][y].m_CompetitionMarker, m_Individuals[x][y].m_NeutralMarker);
      m_Individuals[x][y].m_Species->updateMean();
      m_Individuals[x][y].m_Species->m_FirstComp = m_Individuals[x][y].m_Species->m_CompetitionMean;
      m_Individuals[x][y].m_Species->m_FirstMean = m_Individuals[x][y].m_Species->m_Mean;
      m_Individuals[x][y].m_Species->m_FirstNeutral = m_Individuals[x][y].m_Species->m_NeutralMean;
      //			individuals[x][y].Species->date_of_extinction = runs;
      m_Phylogeny.updatePhylogeny(m_Individuals[x][y].m_Species);


      // density dependence / competition
      if(m_DD)
      {
         //				std::cout << "DD!!" << '\n';
         int focus_x, focus_y, densityKernel_x, densityKernel_y;
         double relatedness;
         double cells = double((m_DensityCutoff * 2 +1) * (m_DensityCutoff * 2 +1) - 1) ;

         for(int relativeX= - m_DensityCutoff; relativeX <= m_DensityCutoff; relativeX++)
         {
            for(int relativeY = - m_DensityCutoff ; relativeY <=  m_DensityCutoff ; relativeY++)
            {

               // get focus cell
               focus_x = ((x + relativeX + m_Xdimensions) % m_Xdimensions);
               focus_y = ((y + relativeY + m_Ydimensions) % m_Ydimensions);

               relatedness = 0.0;

               for(int relativeX2= - m_DensityCutoff; relativeX2 <= m_DensityCutoff; relativeX2++)
               {
                  for(int relativeY2 = - m_DensityCutoff ; relativeY2 <=  m_DensityCutoff ; relativeY2++)
                  {
                     densityKernel_x = ((focus_x + relativeX2 + m_Xdimensions) % m_Xdimensions);
                     densityKernel_y = ((focus_y + relativeY2 + m_Ydimensions) % m_Ydimensions);

                     if (!(densityKernel_x == focus_x && densityKernel_y == focus_y))
                     {
                        relatedness += std::abs(m_Individuals[focus_x][focus_y].m_CompetitionMarker - m_Individuals[densityKernel_x][densityKernel_y].m_CompetitionMarker);
                     }
                  }
               }
               m_Individuals[focus_x][focus_y].m_LocalDensity = relatedness / cells;
            }
         }
      }
   }

}



