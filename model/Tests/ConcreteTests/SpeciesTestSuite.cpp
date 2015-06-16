/*
 * SpeciesTestSuite.cpp
 *
 *  Created on: 26.05.2015
 *      Author:  Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Species.h>
#include <Tests/AbstractTests/TestCase.h>
#include <Tests/ConcreteTests/SpeciesTestSuite.h>
#include <string>
#include <utility>
#include <vector>

using namespace std;
namespace PhyloTest {


SpeciesTestSuite::~SpeciesTestSuite() {
   // TODO Auto-generated destructor stub
}

void SpeciesTestSuite::createTestCases() {
   TestCase* tc = new TestCase("Void-Constructor");
   Species* sp = new Species();

   tc->ASSERT_EQ(string("ID"), sp->m_ID, 0);
   tc->ASSERT_EQ(string("Ancestor"), sp->m_Ancestor, 0);
   tc->ASSERT_EQ(string("Count"), sp->m_Count, 0);
   tc->ASSERT_EQ(string("Date_of_Emergence"), sp->m_Date_of_Emergence, 0);
   tc->ASSERT_EQ(string("Origin"), sp->m_Origin, std::pair<int, int>(0, 0));
   tc->ASSERT_EQ(string("Variance"), sp->m_Variance, 0.04659906);
   tc->ASSERT_EQ(string("Mean"), sp->m_Mean, 1.0);
   tc->ASSERT_EQ(string("FirstMean"), sp->m_FirstMean, 0.5);
   tc->ASSERT_EQ(string("FirstComp"), sp->m_FirstComp, 0.5);
   tc->ASSERT_EQ(string("FirstNeutral"), sp->m_FirstNeutral, 0.5);
   tc->ASSERT_EQ(string("MeanSum"), sp->m_MeanSum, 0.0);
   tc->ASSERT_EQ(string("CompetitionSum"), sp->m_CompetitionSum, 0.0);
   tc->ASSERT_EQ(string("NeutralSum"), sp->m_NeutralSum, 0.0);
   tc->ASSERT_EQ(string("Date_of_Extinction"), sp->m_Date_of_Extinction, 100);
   tc->ASSERT_EQ(string("CompetitionMean"), sp->m_CompetitionMean, 0.5);
   tc->ASSERT_EQ(string("NeutralMean"), sp->m_NeutralMean, 0.5);

   m_TestCases.push_back(tc);

   TestCase* tc1 = new TestCase("Constructor_with_Args");
   Species* sp1 = new Species(1231234, 12312345, 11051988, std::pair<int, int>(42, 12), 123);

   tc1->ASSERT_EQ(string("ID"), sp1->m_ID, 1231234);
   tc1->ASSERT_EQ(string("Ancestor"), sp1->m_Ancestor, 12312345);
   tc1->ASSERT_EQ(string("Count"), sp1->m_Count, 0);
   tc1->ASSERT_EQ(string("Date_of_Emergence"), sp1->m_Date_of_Emergence, 11051988);
   tc1->ASSERT_EQ(string("Origin"), sp1->m_Origin, std::pair<int, int>(42, 12));
   tc->ASSERT_EQ(string("Variance"), sp->m_Variance, 0.04659906);
   tc1->ASSERT_EQ(string("Mean"), sp1->m_Mean, 1.0);
   tc1->ASSERT_EQ(string("FirstMean"), sp1->m_FirstMean, 1.0);
   tc1->ASSERT_EQ(string("FirstComp"), sp1->m_FirstComp, 0.5);
   tc1->ASSERT_EQ(string("FirstNeutral"), sp1->m_FirstNeutral, 0.5);
   tc->ASSERT_EQ(string("MeanSum"), sp->m_MeanSum, 0.0);
   tc->ASSERT_EQ(string("CompetitionSum"), sp->m_CompetitionSum, 0.0);
   tc->ASSERT_EQ(string("NeutralSum"), sp->m_NeutralSum, 0.0);
   tc1->ASSERT_EQ(string("Date_of_Extinction"), sp1->m_Date_of_Extinction, 123);
   tc1->ASSERT_EQ(string("CompetitionMean"), sp1->m_CompetitionMean, 0.5);
   tc1->ASSERT_EQ(string("NeutralMean"), sp1->m_NeutralMean, 0.5);

   m_TestCases.push_back(tc1);

//   void Species::sumMean(double env, double comp, double neutral){
//      m_MeanSum += env;
//      m_CompetitionSum += comp;
//      m_NeutralSum += neutral;
//   }
   TestCase* tc2 = new TestCase("Sum_Mean_Method");

   sp->sumMean(3.7, -5.3, -42.7);
   sp1->sumMean(11.123, 0.0, 0.42);

   tc2->ASSERT_EQ(string("MeanSum"), sp->m_MeanSum, 3.7);
   tc2->ASSERT_EQ(string("CompetitionSum"), sp->m_CompetitionSum, -5.3);
   tc2->ASSERT_EQ(string("NeutralSum"), sp->m_NeutralSum, -42.7);

   tc2->ASSERT_EQ(string("MeanSum"), sp1->m_MeanSum, 11.123);
   tc2->ASSERT_EQ(string("CompetitionSum"), sp1->m_CompetitionSum, 0.0);
   tc2->ASSERT_EQ(string("NeutralSum"), sp1->m_NeutralSum, 0.42);


   m_TestCases.push_back(tc2);

   TestCase* tc3 = new TestCase("Dec_Mean_Method");

   sp->decMean(3.7, -5.3, -42.7);
   sp1->decMean(11.123, 0.0, 0.42);

   tc3->ASSERT_EQ(string("MeanSum"), sp->m_MeanSum, 0.0);
   tc3->ASSERT_EQ(string("CompetitionSum"), sp->m_CompetitionSum, -10.6);
   tc3->ASSERT_EQ(string("NeutralSum"), sp->m_NeutralSum, -85.4);

   tc3->ASSERT_EQ(string("MeanSum"), sp1->m_MeanSum, 0.0);
   tc3->ASSERT_EQ(string("CompetitionSum"), sp1->m_CompetitionSum, 0.0);
   tc3->ASSERT_EQ(string("NeutralSum"), sp1->m_NeutralSum, 0.0);


   m_TestCases.push_back(tc3);
}

} /* namespace PhyloTest */
