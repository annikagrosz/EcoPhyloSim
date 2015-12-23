/*
 * phylogeny.cpp
 *
 *  Created on: 30.07.2014
 *      Author: Paul
 */

#include "Phylogeny.h"
#include "StringConversion.h"

#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <stdexcept>
#include <utility>

Phylogeny::Phylogeny() {
   m_PrunedPhylo = new std::multimap<unsigned long long, Species*>();
   m_FullPhylogeny = new std::multimap<unsigned long long, Species*>();
}

Phylogeny::~Phylogeny(){}


void Phylogeny::updatePhylogeny(Species * spec)
{
	m_FullPhylogeny->insert(std::make_pair(spec->m_ID, spec));
}


void Phylogeny::writeSpeciesData()
{
	std::ofstream specData("..\\species_data.txt");
	specData << "ID, nChildren, environment, competition, neutral, emergence, extinction, ancestor, count" << '\n';
	Species * spec;
	for (unsigned int i = 1; i < m_FullPhylogeny->size(); i++)
	{
		spec = m_FullPhylogeny->find(i)->second;
		specData << spec->m_ID << ',' << spec->m_Children.size() << ','  << spec->m_Mean << ',' ;
		specData  << spec->m_CompetitionMean << ',' << spec->m_NeutralMean << ',' << spec->m_Date_of_Emergence << ',' << spec->m_Date_of_Extinction << ',' << spec->m_Ancestor << ',' << spec->m_Count << '\n';
	}

}


void Phylogeny::prunePhylogeny(int current)
{
  std::cout << "Prune Phylogeny" << std::endl;
  m_PrunedPhylo->clear();

  // Create deep copy
  // TODO -> I tried the deep copy of multimap, but it doesn't work ... sort this out!!!
  for(unsigned long long i=1; i <= m_FullPhylogeny->size(); i++){
    Species * specp = new Species(*m_FullPhylogeny->find(i)->second);
    if (specp->m_Date_of_Extinction > current) specp->m_Date_of_Extinction = current;
    m_PrunedPhylo->insert( std::pair<unsigned long long, Species*>(specp->m_ID, specp));

  }


  // TODO - I somehow don't understand 100% why this works .. we go through the phylogeny size, erase things inbetween, why don't we get into 
  // trouble for trying to access indices that have already been erase?
  
	for(unsigned long long i=1; i <= m_PrunedPhylo->size(); i++)
	{


		Species * father = m_PrunedPhylo->find(i)->second;

		for(int i = 0; i<10; i++){
			if(!(father->m_Children.empty()))
			{
				 for(unsigned int j=0; j<  father->m_Children.size(); j++)
				 {
					 Species * child = m_PrunedPhylo->find(father->m_Children[j])->second;
					 if(child->m_Children.empty() && child->m_Count==0)
					 {
						 father->m_Children.erase(father->m_Children.begin()+j);
					 }
	//				 else if(!(child->children.empty()) && child->count==0)
	//				 {
	//					 father->children.erase(father->children.begin()+j);
	//					 father->children.insert(father->children.end(), child->children.begin(), child->children.end());
	//				 }
				 }
			}
		}
		// std::cout << father->children.size() << '\n';
	}
	// std::cout <<"phylosize : " << prunedPhylo.size() << '\n';
}

void Phylogeny::writePhylogeny(unsigned long long start, std::multimap<unsigned long long, Species*> *phylogenyMap, char suffix)
{
	 std::cout << "write Phylogeny" << std::endl;
	Species * parent = phylogenyMap->find(start)->second;
	Species * ancestorSpecies = new Species(0,0,0,std::make_pair(0,0),0);
	phylogenyMap->insert(std::make_pair(ancestorSpecies->m_ID, ancestorSpecies));
	parent->m_Ancestor = 0;
	//std::map<unsigned long long, int> position; // Initialisieren ??
//	std::vector <unsigned long long> phylchildren;

//	unsigned long long child;
//	unsigned int count = 0;
	int branchLength = 0;
//	int rootLength = 0;
//	double parentBranchlength = 0.0;
//	unsigned long long ancestor = 0;

//	phylchildren = parent->children;
	//position[parent->ID] = 0 ;


	std::cout << "Printing Phylogeny to file. Starting point is species " << start << '\n';
	std::cout << "size of tree : "<< phylogenyMap->size() << '\n' ;


	std::stringstream filename;
	filename << "..\\phylo_newick" << suffix << ".txt" ;
	std::ofstream phylo(filename.str().c_str());

	std::string tree = ";" ;


	std::vector <size_t> position(phylogenyMap->size(),0);
	std::vector <size_t> paranthesis(phylogenyMap->size(),0);


   bool go = true;
	while(go )
	{

//		if(position[parent->ID]+1 <= parent->children.size() )
//		{


			// todo rename paerent to focusSpecies
		    // todo long term: think about changing children from integer vector + map to pointer


			// If parent is leaf, write down leaf and length and go one up
			if(parent->m_Children.empty())
			{
				if(position[parent->m_ID] != 0) throw std::runtime_error("unexpected value for position value in a leaf") ;
				branchLength = parent->m_Date_of_Extinction - parent->m_Date_of_Emergence;
				tree.insert(0, to_string(branchLength) );
				tree.insert(0, ":" );
				tree.insert(0, to_string(parent->m_ID) );
				tree.insert(0, "s" );
				tree.insert(0, "," );
				parent = phylogenyMap->find(parent->m_Ancestor)->second ;
				position[parent->m_ID] += 1;
			}

			// if all children visited write down parent, close and go one up
			else if(position[parent->m_ID] == parent->m_Children.size() )
			{
				branchLength = parent->m_Date_of_Extinction - phylogenyMap->find(parent->m_Children.back())->second->m_Date_of_Emergence;

				tree.insert(0, to_string(branchLength));
				tree.insert(0, ":");
				tree.insert(0, to_string(parent->m_ID));
				tree.insert(0, "s" );
				tree.insert(0, paranthesis[parent->m_ID],'(' );
				tree.insert(0, "," );
				parent = phylogenyMap->find(parent->m_Ancestor)->second;

				position[parent->m_ID] +=1 ;

			}

			// If Parent has children and not yet gone to each child
			// then go to child
			else if(position[parent->m_ID] < parent->m_Children.size() )
			{

				// if two siblings are born in the same generation

				if ( position[parent->m_ID] > 1 &&
					phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]-1])->second->m_Date_of_Emergence &&
					phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]+1])->second->m_Date_of_Emergence){
					if (!(phylogenyMap->find(parent->m_Children[position[parent->m_ID] - 1])->second->m_Children.empty()) && (phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Children.empty())){

						;
//						tree.insert(0, "," );
					}
				}
				else{
					// if first time going through this node branch lenght is distance to parent node
					if(position[parent->m_ID] == 0)
					{
						branchLength = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence - parent->m_Date_of_Emergence;
					}
					// else branch lenght is distance to previous sibling
					else // ????? or position = children size
					{
						branchLength = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence - phylogenyMap->find(parent->m_Children[position[parent->m_ID ]-1])->second->m_Date_of_Emergence;
					}

					tree.insert(0, to_string(branchLength));
					tree.insert(0, ":");
					tree.insert(0, to_string(parent->m_ID));
					tree.insert(0, "s");
					tree.insert(0, ")");
					paranthesis[parent->m_ID] += 1;
				}

				parent = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second;
			}
			else throw std::runtime_error("Nope");


		if (parent->m_ID == 0)
		{
			go = false ;
		}
	}
	tree.erase(tree.begin());
	std::cout << phylogenyMap->find(1)->second->m_Date_of_Extinction << '\n';
	std::cout << phylogenyMap->find(phylogenyMap->find(1)->second->m_Children.back())->second->m_Date_of_Emergence << '\n';
	phylo << tree;
}


std::string Phylogeny::writePhylogenyR(unsigned long long start, std::multimap<unsigned long long, Species*> * phylogenyMap)
{

	 std::cout << "Write Phylogeny" << std::endl;
	Species * parent = phylogenyMap->find(start)->second;
	Species * ancestorSpecies = new Species(0,0,0,std::make_pair(0,0),0);
	phylogenyMap->insert(std::make_pair(ancestorSpecies->m_ID, ancestorSpecies));
	parent->m_Ancestor = 0;

	int branchLength = 0;

	std::string tree = ";" ;
	std::vector <size_t> position  (phylogenyMap->size(),0);
	std::vector <size_t> paranthesis  (phylogenyMap->size(),0);
	int counter = 1;

    bool go = true;
	while(go )
	{


			// todo rename paerent to focusSpecies
		    // todo long term: think about changing children from integer vector + map to pointer


			// If parent is leaf, write down leaf and length and go one up
			if(parent->m_Children.empty())
			{
				std::cout << "Children empty" << std::endl;
				if(position[parent->m_ID] != 0) throw std::runtime_error("unexpected value for position value in a leaf") ;
				branchLength = parent->m_Date_of_Extinction - parent->m_Date_of_Emergence;
				tree.insert(0, to_string(branchLength) );
				tree.insert(0, ":" );
				tree.insert(0, to_string(parent->m_ID) );
				tree.insert(0, "s" );
				tree.insert(0, "," );
				parent = phylogenyMap->find(parent->m_Ancestor)->second ;
				position[parent->m_ID] += 1;
				std::cout << "Children empty found ancestor" << std::endl;
			}


			// if all children visited write down parent, close and go one up
			else if(position[parent->m_ID] == parent->m_Children.size() )
			{

				std::cout << "Children all visited" << std::endl;
				branchLength = parent->m_Date_of_Extinction - phylogenyMap->find(parent->m_Children.back())->second->m_Date_of_Emergence;

				tree.insert(0, to_string(branchLength));
				tree.insert(0, ":");
				tree.insert(0, to_string(parent->m_ID));
				tree.insert(0, "s" );
				tree.insert(0, paranthesis[parent->m_ID],'(' );
				tree.insert(0, "," );
				parent = phylogenyMap->find(parent->m_Ancestor)->second;
				position[parent->m_ID] +=1 ;

				std::cout << "All children visited found ancestor" << std::endl;

			}

			// If Parent has children and not yet gone to each child
			// then go to child
			else if(position[parent->m_ID] < parent->m_Children.size() )
			{
				std::cout << "Children not all visited yet" << std::endl;
				// if two siblings are born in the same generation
				//if(position[parent->m_ID]==3) throw std::runtime_error("Cannot build phylogeny");

				std::cout << "Position " << position[parent->m_ID] << std::endl;

         // Original Verrsion
				/*if ( position[parent->m_ID] > 1 &&
					phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]-1])->second->m_Date_of_Emergence &&
					phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]+1])->second->m_Date_of_Emergence){

					std::cout << "IF" << std::endl;
					if (!(phylogenyMap->find(parent->m_Children[position[parent->m_ID] - 1])->second->m_Children.empty()) && (phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Children.empty())){
						std::cout << "Found parent3" << std::endl;
						;
//						tree.insert(0, "," );
					}
				}*/


				// New version needed to find the bug.

				if ( position[parent->m_ID] > 1){

					 std::cout<<"Self: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence  << std::endl;
					 std::cout<<"Minus one: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]-1])->second->m_Date_of_Emergence  << std::endl;

					 if(phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]-1])->second->m_Date_of_Emergence){
						std::cout << "Found first parent" << std::endl;
						 std::cout<<"Self: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence << std::endl;
						 //std::cout<<"Plus one: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]+1])->second->m_Date_of_Emergence  << std::endl;

						 std::cout<<"Children m_ID: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_ID  << std::endl;
						 std::cout<<"Children2 m_ID: " << phylogenyMap->find(parent->m_Children[position[parent->m_ID]+1])->second->m_ID  << std::endl;


						 std::cout<<"Position: " << position[parent->m_ID]  << std::endl;

						 std::cout<<"Parent m_ID: " <<parent->m_ID << std::endl;
						 std::cout<<"Length Position: " << position.size() << std::endl;





						 // Here is the bug (or here shows the bug): The problem is, that plus one cannot be found.


						if(phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence == phylogenyMap->find(parent->m_Children[position[parent->m_ID]+1])->second->m_Date_of_Emergence){
							std::cout << "Found second parent" << std::endl;
							if (!(phylogenyMap->find(parent->m_Children[position[parent->m_ID] - 1])->second->m_Children.empty()) && (phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Children.empty())){
							std::cout << "Found parent3" << std::endl;
								;
	//						tree.insert(0, "," );
					  }
					}
				}
			}




				//End new/test version



				else{
					std::cout << "ELSE" << std::endl;
					// if first time going through this node branch lenght is distance to parent node
					if(position[parent->m_ID] == 0)
					{
						branchLength = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence - parent->m_Date_of_Emergence;
						std::cout << "Found parent 1" << std::endl;

					}
					// else branch lenght is distance to previous sibling
					else // ????? or position = children size
					{
						std::cout << "ELSE 2" << std::endl;
						branchLength = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second->m_Date_of_Emergence - phylogenyMap->find(parent->m_Children[position[parent->m_ID ]-1])->second->m_Date_of_Emergence;
						std::cout << "Found parent 2" << std::endl;
					}

					tree.insert(0, to_string(branchLength));
					tree.insert(0, ":");
					tree.insert(0, to_string(parent->m_ID));
					tree.insert(0, "s");
					tree.insert(0, ")");
					paranthesis[parent->m_ID] += 1;
				}

				parent = phylogenyMap->find(parent->m_Children[position[parent->m_ID]])->second;
				std::cout << "Found parent" << std::endl;
			}

			else break;
			//else throw std::runtime_error("Nope");

			counter +=1;
			//std::cout << "Counter: " << counter << " Size: " << phylogenyMap->size() << std::endl;
			//std::cout << "Position max: " << *std::max_element(position.begin(), position.end()) << std::endl;

		if(counter > phylogenyMap->size()) break;

		if (parent->m_ID == 0)
		{
			go = false ;
		}
	}
	//std::cout << tree << std::endl;

	tree.erase(tree.begin());
	return tree;
}
