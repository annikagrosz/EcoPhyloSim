/*
 * TestSuiteCollection.h
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef ABSTRACTTESTS_TESTSUITECOLLECTION_H_
#define ABSTRACTTESTS_TESTSUITECOLLECTION_H_

#include <Tests/AbstractTests/TestSuite.h>
#include <vector>


namespace PhyloTest {
class TestSuiteCollection {
public:
   TestSuiteCollection();
   virtual ~TestSuiteCollection();
   static void collectTestSuites();
   static void runTestSuites();
private:
   static std::vector<TestSuite*> m_TestSuites;
};

} /* namespace PhyloTest */
#endif /* ABSTRACTTESTS_TESTSUITECOLLECTION_H_ */
