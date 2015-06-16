/*
 * IndividualTestSuite.cpp
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Individual.h>
#include <Tests/AbstractTests/TestCase.h>
#include <Tests/ConcreteTests/IndividualTestSuite.h>
#include <cmath>
#include <string>
#include <vector>

using namespace std;

namespace PhyloTest {

//IndividualTestSuite::IndividualTestSuite() : public TestSuite() {
//   // TODO Auto-generated constructor stub
//
//}

IndividualTestSuite::~IndividualTestSuite() {
   // TODO Auto-generated destructor stub
}



void IndividualTestSuite::createTestCases() {
   // Test the constructor
   TestCase* tc = new TestCase("Constructor");
   Individual ind;
   tc->ASSERT_TRUE(string("Species_is_not_null"), ind.m_Species != NULL);

   tc->ASSERT_EQ(string("x_coordinate"), ind.m_X_coordinate, 0);
   tc->ASSERT_EQ(string("y_coordinate"), ind.m_Y_coordinate, 0);
   tc->ASSERT_EQ(string("m_LocalDensity"), ind.m_LocalDensity, 0.0);
   tc->ASSERT_EQ(string("m_Age"), ind.m_Age, 0);
   tc->ASSERT_EQ(string("m_FitnessWeight"), ind.m_FitnessWeight, 0.5);
   tc->ASSERT_EQ(string("m_DensityStrength"), ind.m_DensityStrength, 0.4);
   tc->ASSERT_EQ(string("m_Weight"), ind.m_Weight, 1);
   tc->ASSERT_EQ(string("m_Variance"), ind.m_Variance, 0.04659906);
   tc->ASSERT_EQ(string("m_Mean"), ind.m_Mean, 1.0);
   tc->ASSERT_EQ(string("m_CompetitionMarker"), ind.m_CompetitionMarker, 0.5);
   tc->ASSERT_EQ(string("m_NeutralMarker"), ind.m_NeutralMarker, 0.5);
   m_TestCases.push_back(tc);
   // Test the method dispersal
   TestCase* tc1 = new TestCase("dispersal-method");
   tc1->ASSERT_EQ(string("nearest_neighbour(type=2)"), ind.dispersal(2, 2.0), 0.0);
   tc1->ASSERT_EQ(string("nearest_neighbour(type=2)"), ind.dispersal(2, 0.5), 1.0);
   tc1->ASSERT_EQ(string("nearest_neighbour(type=3)"), ind.dispersal(3, 2.0), exp(-1.0));
   m_TestCases.push_back(tc1);

   // TODO(Betim): 2 methods not tested (getSeedsTo, evolution)

}

} /* namespace PhyloTest */
