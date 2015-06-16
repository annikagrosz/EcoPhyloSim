/*
 * SpeciesTestSuite.h
 *
 *  Created on: 26.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef CONCRETETESTS_SPECIESTESTSUITE_H_
#define CONCRETETESTS_SPECIESTESTSUITE_H_

#include <Tests/AbstractTests/TestSuite.h>

namespace PhyloTest {

class SpeciesTestSuite: public TestSuite {
public:
   explicit SpeciesTestSuite() : TestSuite("SpeciesTestSuite") {

   }
   virtual ~SpeciesTestSuite();
protected:
   void createTestCases();
};

} /* namespace PhyloTest */

#endif /* CONCRETETESTS_SPECIESTESTSUITE_H_ */
