/*
 * TestCase.cpp
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Tests/AbstractTests/TestCase.h>


namespace PhyloTest {

TestCase::TestCase(const char* testCaseName) {
   m_TestCaseName = testCaseName;
}

TestCase::~TestCase() {
   // TODO(Betim): Implement destructor!
}

void TestCase::ASSERT_EQ(std::string nameOfAssertion,
      unsigned long long CURRENT_VALUE, unsigned long long EXPECTED_VALUE) {
   TestCaseResult tcr;
   tcr.AssertionName = string(nameOfAssertion.c_str());
   CURRENT_VALUE == EXPECTED_VALUE ? tcr.PASSED = 1 : tcr.PASSED = 0;
   tcr.FailureInfo = generateFailureInfo(CURRENT_VALUE, EXPECTED_VALUE);
   m_TestCaseResults.push_back(tcr);
}

void TestCase::ASSERT_TRUE(std::string nameOfAssertion, bool b) {
   TestCaseResult tcr;
   tcr.AssertionName = nameOfAssertion;
   b? tcr.PASSED = 1 : tcr.PASSED = 0;
   m_TestCaseResults.push_back(tcr);
}



}

