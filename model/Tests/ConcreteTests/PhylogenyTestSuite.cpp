/*
 * PhylogenyTestSuite.cpp
 *
 *  Created on: 26.05.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <Phylogeny.h>
#include <Species.h>
#include <Tests/AbstractTests/TestCase.h>
#include <Tests/ConcreteTests/PhylogenyTestSuite.h>
#include <map>
#include <string>
#include <vector>

using namespace std;

namespace PhyloTest {


PhylogenyTestSuite::~PhylogenyTestSuite() {
   // TODO Auto-generated destructor stub
}

void PhylogenyTestSuite::createTestCases() {
   TestCase* tc = new TestCase("Void-Constructor");
   Phylogeny* pg = new Phylogeny();
   tc->ASSERT_TRUE(string("FullPhylogeny_Not_Null"), pg->m_FullPhylogeny != NULL);
   tc->ASSERT_TRUE(string("PrunedPhylogeny_Not_Null"), pg->m_PrunedPhylo != NULL);
   m_TestCases.push_back(tc);

   TestCase* tc1 = new TestCase("UpdatePhylogeny_Method");
   Species* sp = new Species();
   sp->m_ID = 123456L;
   pg->updatePhylogeny(sp);

   tc1->ASSERT_EQ(string("Inserted_Elem_is_Found"),
         pg->m_FullPhylogeny->count(sp->m_ID), 1);

   pg->updatePhylogeny(sp);

   tc1->ASSERT_EQ(string("Inserted_Elem_occurs_Twice"),
         pg->m_FullPhylogeny->count(sp->m_ID), 2);
   unsigned long idThatOccursNot = 10023;
   tc1->ASSERT_EQ(string("Elem_occurs_Not"),
            pg->m_FullPhylogeny->count(idThatOccursNot), 0);
   tc1->ASSERT_EQ(string("Elem_occurs_Not2"),
               pg->m_FullPhylogeny->count(-idThatOccursNot), 0);
   m_TestCases.push_back(tc1);

}

} /* namespace PhyloTest */
