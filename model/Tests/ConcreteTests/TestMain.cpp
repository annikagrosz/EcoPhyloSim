/*
 * TestMain.cpp
 *
 *  Created on: 22.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Tests/AbstractTests/TestSuiteCollection.h>

int main(int argc, char** argv) {
   PhyloTest::TestSuiteCollection::collectTestSuites();
   PhyloTest::TestSuiteCollection::runTestSuites();
}
