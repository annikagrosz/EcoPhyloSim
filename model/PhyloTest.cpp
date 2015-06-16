/*
 * PhyloTest.cpp
 *
 *  Created on: 13.10.2014
 *      Author: Paul
 */

#include "phylogeny.h"
#include "species.h"


int main()
{
	phylogeny Phylo;
	species * spec1 = new species(1,1,0, std::make_pair(1,1));
	spec1->date_of_extinction = 10;
	spec1->count +=1;
	Phylo.updatePhylogeny(spec1);

	species * spec2 = new species(2,1,1, std::make_pair(1,1));
	spec2->date_of_extinction = 2;
	spec1->children.push_back(spec2->ID);
	Phylo.updatePhylogeny(spec2);

	species * spec4 = new species(4,2,2, std::make_pair(1,1));
	spec4->date_of_extinction = 10;
	spec4->count +=1;
	spec2->children.push_back(spec4->ID);
	Phylo.updatePhylogeny(spec4);

	species * spec5 = new species(5,1,2, std::make_pair(1,1));
	spec5->date_of_extinction = 8;
	spec1->children.push_back(spec5->ID);
	Phylo.updatePhylogeny(spec5);

	species * spec3 = new species(3,1,2, std::make_pair(1,1));
	spec3->date_of_extinction = 10;
	spec3->count +=1;
	spec1->children.push_back(spec3->ID);
	Phylo.updatePhylogeny(spec3);

	species * spec6 = new species(6,3,3, std::make_pair(1,1));
	spec6->date_of_extinction = 8;
	spec6->count +=0;
	spec3->children.push_back(spec6->ID);
	Phylo.updatePhylogeny(spec6);

	species * spec9 = new species(9,5,5, std::make_pair(1,1));
	spec9->date_of_extinction = 10;
	spec9->count +=1;
	spec5->children.push_back(spec9->ID);
	Phylo.updatePhylogeny(spec9);

	species * spec7 = new species(7,1,6, std::make_pair(1,1));
	spec7->date_of_extinction = 10;
	spec7->count +=1;
	spec1->children.push_back(spec7->ID);
	Phylo.updatePhylogeny(spec7);

	species * spec10 = new species(10,9,7, std::make_pair(1,1));
	spec10->date_of_extinction = 10;
	spec10->count +=1;
	spec9->children.push_back(spec10->ID);
	Phylo.updatePhylogeny(spec10);

	species * spec8 = new species(8,7,8, std::make_pair(1,1));
	spec8->date_of_extinction = 10;
	spec8->count +=1;
	spec7->children.push_back(spec8->ID);
	Phylo.updatePhylogeny(spec8);


	Phylo.prunePhylogeny(Phylo.fullPhylogeny);

//	for(int k=1; k <= Phylo.prunedPhylo.size(); k++){
//	std::cout <<"ID : " << Phylo.prunedPhylo.find(k)->second->ID << '\n';
//	}

	Phylo.writePhylogeny(1,3, Phylo.prunedPhylo);



	return 0;
}


