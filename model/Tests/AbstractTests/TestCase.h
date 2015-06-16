/*
 * TestCase.h
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef ABSTRACTTESTS_TESTCASE_H_
#define ABSTRACTTESTS_TESTCASE_H_

#include <string>
#include <vector>

using namespace std;


namespace PhyloTest {

struct TestCaseResult {
  bool PASSED;
  string AssertionName;
  string FailureInfo;
};

/**
 * A test case represents a test of a method or a member variable.
 * E.g. if one wants to test a method, then you usually call it with different values and examine
 * the returned result.
 * A test case is part of a test suite.
 */
class TestCase {
public:
   explicit TestCase(const char* testCaseName);
   virtual ~TestCase();

   // Test the equality between two values. The current value is the value returned from
   // a call to a method or look-up of a member variable.  The expected value is a concrete value
   // that one expects to get from the method call or member look-up.
   // The test passes if current value is equal to the expected value, otherwise it fails.
   template<typename T> void ASSERT_EQ(string nameOfAssertion, const T& CURRENT_VALUE,
         const T& EXPECTED_VALUE);

   void ASSERT_EQ(string nameOfAssertion,
         unsigned long long v1, unsigned long long v2);

   // Assert that the given boolean value 'b' evaluates to 'TRUE'.
   // The test passes if the 'b' evaluates to 'TRUE', otherwise it fails.
   void ASSERT_TRUE(string nameOfAssertion, bool b);

   const vector<TestCaseResult>& getTestCaseResults() const {
      return m_TestCaseResults;
   }

   const char* getTestCaseName() const {
      return m_TestCaseName;
   }

private:
   const char* m_TestCaseName;
   vector<TestCaseResult> m_TestCaseResults;
   template<typename T> string generateFailureInfo(const T& CURRENT_VALUE,
         const T& EXPECTED_VALUE);
};

template<typename T> void TestCase::ASSERT_EQ(string nameOfAssertion, const T& CURRENT_VALUE,
      const T& EXPECTED_VALUE) {
   TestCaseResult tcr;
   tcr.AssertionName = string(nameOfAssertion.c_str());
   CURRENT_VALUE == EXPECTED_VALUE ? tcr.PASSED = 1 : tcr.PASSED = 0;
   tcr.FailureInfo = generateFailureInfo(CURRENT_VALUE, EXPECTED_VALUE);
   m_TestCaseResults.push_back(tcr);
}

template<typename T> string TestCase::generateFailureInfo(const T& CURRENT_VALUE,
      const T& EXPECTED_VALUE) {
//   stringstream ss;
//   ss << "(EXPECTED: " << to_string(EXPECTED_VALUE);
//   ss << ", IS: " << to_string(CURRENT_VALUE) << ")" << endl;
//   return ss.str();
   return string(" (TODO_FailureInfo)");
}

} /* namespace PhyloTest */



#endif /* ABSTRACTTESTS_TESTCASE_H_ */
