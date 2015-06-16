/*
 * TestSuite.h
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef ABSTRACTTESTS_TESTSUITE_H_
#define ABSTRACTTESTS_TESTSUITE_H_

#include <Tests/AbstractTests/TestCase.h>
#include <vector>

using namespace std;

namespace PhyloTest {
/**
 * A test suite represents the test of a class. A class usually consists of several methods, the
 * test of each method is a test case, and all the test cases together represent a test suite.
 */
class TestSuite {
public:
   explicit TestSuite(const char* testSuiteName);
   virtual ~TestSuite();
   void runTestSuite();
   void printTestSuiteName();
   void printStatisticalInformation();

protected:
   const char* m_TestSuiteName;
   vector<TestCase*> m_TestCases;

   virtual void createTestCases() = 0;

private:
   // Executes each test case and evaluates its output.
   void evaluateAndOutputTheTestCases();

   // Member
   size_t m_TestCasesPassed;
   size_t m_SumOfAssertionsPassed;
   size_t m_TotalAssertions;
};

} /* namespace PhyloTest */
#endif /* ABSTRACTTESTS_TESTSUITE_H_ */
