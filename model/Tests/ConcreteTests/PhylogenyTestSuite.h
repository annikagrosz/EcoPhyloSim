/*
 * PhylogenyTestSuite.h
 *
 *  Created on: 26.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef CONCRETETESTS_PHYLOGENYTESTSUITE_H_
#define CONCRETETESTS_PHYLOGENYTESTSUITE_H_

#include <Tests/AbstractTests/TestSuite.h>

namespace PhyloTest {

class PhylogenyTestSuite: public TestSuite {
public:
   explicit PhylogenyTestSuite() : TestSuite("PhylogenyTestSuite") {

   }
   virtual ~PhylogenyTestSuite();
protected:
   void createTestCases();
};

} /* namespace PhyloTest */

#endif /* CONCRETETESTS_PHYLOGENYTESTSUITE_H_ */
