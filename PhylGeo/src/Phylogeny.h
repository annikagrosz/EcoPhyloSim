/*
 * phylogeny.h
 *
 *  Created on: 30.07.2014
 *      Author: Paul
 */

#ifndef PHYLOGENY_H_
#define PHYLOGENY_H_


#include <map>
#include <string>
#include <vector>

#include "Species.h"


class Phylogeny {
 public:
	Phylogeny();
	~Phylogeny();
	std::multimap<unsigned long long, Species*> *m_FullPhylogeny;
	std::multimap<unsigned long long, Species*> *m_PrunedPhylo;
	void updatePhylogeny(Species * spec);

	void writePhylogeny(unsigned long long start, 
  std::multimap<unsigned long long, Species*> *phylogenyMap, char suffix);

	std::string writePhylogenyR(unsigned long long start, 
	      std::multimap<unsigned long long, Species*> *phylogenyMap);

	void writeSpeciesData();
	void prunePhylogeny(int current);
	std::vector<Species*> specVec;
};

#endif /* PHYLOGENY_H_ */
