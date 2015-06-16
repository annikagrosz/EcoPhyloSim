/*
 * TestSuiteRegister.cpp
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Tests/AbstractTests/TestSuiteCollection.h>
#include <Tests/ConcreteTests/IndividualTestSuite.h>
#include <Tests/ConcreteTests/PhylogenyTestSuite.h>
#include <Tests/ConcreteTests/SpeciesTestSuite.h>

namespace PhyloTest {
std::vector<TestSuite*> TestSuiteCollection::m_TestSuites;

TestSuiteCollection::TestSuiteCollection() {
   // TODO Auto-generated constructor stub

}

TestSuiteCollection::~TestSuiteCollection() {
   TestSuiteCollection::m_TestSuites.clear();
}

void TestSuiteCollection::runTestSuites() {
   for (size_t i = 0; i < TestSuiteCollection::m_TestSuites.size(); i++) {
      TestSuiteCollection::m_TestSuites[i]->printTestSuiteName();
      TestSuiteCollection::m_TestSuites[i]->runTestSuite();
      TestSuiteCollection::m_TestSuites[i]->printStatisticalInformation();
   }
}
}

void PhyloTest::TestSuiteCollection::collectTestSuites() {
   TestSuiteCollection::m_TestSuites.push_back(new IndividualTestSuite());
   TestSuiteCollection::m_TestSuites.push_back(new SpeciesTestSuite());
   TestSuiteCollection::m_TestSuites.push_back(new PhylogenyTestSuite());
}
