/*
 * IndividualTestSuite.h
 *
 *  Created on: 22.05.2015
 *      Author: hiwi0101
 */

#ifndef CONCRETETESTS_INDIVIDUALTESTSUITE_H_
#define CONCRETETESTS_INDIVIDUALTESTSUITE_H_

#include <Tests/AbstractTests/TestSuite.h>

namespace PhyloTest {

class IndividualTestSuite : public TestSuite {
public:
   explicit IndividualTestSuite() : TestSuite("IndividualTestSuite") {
   }
   virtual ~IndividualTestSuite();
protected:
   void createTestCases();
};

} /* namespace PhyloTest */

#endif /* CONCRETETESTS_INDIVIDUALTESTSUITE_H_ */
