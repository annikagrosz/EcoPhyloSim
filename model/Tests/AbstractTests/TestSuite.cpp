/*
 * TestSuite.cpp
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Tests/AbstractTests/TestCase.h>
#include <Tests/AbstractTests/TestSuite.h>

#include <vector>
#include <string>
#include <iostream>
#include <sstream>

using namespace std;

namespace PhyloTest {

TestSuite::TestSuite(const char* testSuiteName) {
   m_TestSuiteName = testSuiteName;
   m_TestCasesPassed = 0;
   m_SumOfAssertionsPassed = 0;
   m_TotalAssertions = 0;
//   TestSuiteCollection::addTestSuite(this);
}

TestSuite::~TestSuite() {
   // TODO(Betim): implement destructor!
}

void TestSuite::runTestSuite() {
   createTestCases();
   evaluateAndOutputTheTestCases();
}

void TestSuite::printTestSuiteName() {
   cout << "Starting " << m_TestSuiteName << endl;
}

void TestSuite::printStatisticalInformation() {
   cout << "#Total test cases: " << m_TestCases.size() << endl;
   cout << "#Total assertions: " << m_TotalAssertions << endl;
   cout << "#Test cases PASSED: " << m_TestCasesPassed << endl;
   cout << "#Assertions PASSED: " << m_SumOfAssertionsPassed << endl;
}

void TestSuite::evaluateAndOutputTheTestCases() {
   // Init member for statistics
   m_TestCasesPassed = 0;
   m_SumOfAssertionsPassed = 0;
   m_TotalAssertions = 0;

   // Evaluate the test cases
   for (size_t i = 0; i < m_TestCases.size(); i++) {
      size_t assertionsPassed = 0;
      vector<TestCaseResult> testCaseResults = m_TestCases[i]->getTestCaseResults();
      // Print out the name of the test case
      stringstream ss;
      ss << "Test case: " << m_TestSuiteName << "->" << m_TestCases[i]->getTestCaseName();
      cout << ss.str() << endl;

      // Evaluate each assertions of the current test case.
      for (size_t j = 0; j < testCaseResults.size(); j++) {
         string assertionText("Assertion: ");
         assertionText = assertionText.append(testCaseResults[j].AssertionName);
         cout << assertionText ;
         if (testCaseResults[j].PASSED) {
            cout << "..." << "PASSED" << endl;
         } else {
            cout << "..." << "FAILED" << testCaseResults[j].FailureInfo <<endl;
         }

         // For the statistics
         if (testCaseResults[j].PASSED )  ++assertionsPassed;
      }
      if (assertionsPassed == testCaseResults.size()) {
         // If all assertions of a test case passed, then the whole test case passes.
         ++m_TestCasesPassed;
         // Print message that all assertions passed!
         cout << "All assertions passed!" << endl;
      } else {
         cout << assertionsPassed << " PASSED";
         cout << endl;
         cout << testCaseResults.size() - assertionsPassed << " FAILED";
         cout << endl;
      }
      m_SumOfAssertionsPassed += assertionsPassed;
      m_TotalAssertions += testCaseResults.size();
   }
}

}
