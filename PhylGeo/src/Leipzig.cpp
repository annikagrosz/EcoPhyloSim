/*
 * neighbour.c++
 *
 *  Created on: 01.04.2014
 *      Author: Paul
 */


#include <math.h>
#include <algorithm>

#include "./RandomGen.h"
//#include "./Utils/RandomGen.h"

class leipzigModel
{
	public:

	int length_y ;
	int length_x ;
	unsigned long long global_species_counter;
	RandomGen function;

	leipzigModel(int, int, unsigned long long, int, bool, double);

	const static int l_x =256;
	const static int l_y =256;
	static int speciation_rate;
	static double density_strength;
	static bool density;

	double kernel(int x_center, int y_center, int x_coord, int y_coord)
	{
	int x = abs(x_center - x_coord);
	int y = abs(y_center - y_coord);
	double distance = sqrt(x*x + y*y);
	double chance = exp(1/distance);

	return chance;
	}


	struct individual
	{
		unsigned long long species;
		int x_coordinate;
		int y_coordinate;
		double local_density;
		unsigned long long age;
		double dispersal;
	};

	struct arraystruct { // definition as struct makes return of the array possible
		arraystruct(int, int);
		individual area[l_x][l_y];
	};


	arraystruct buildarea() // build area function provides a way to set up the array
	{
		arraystruct ret(length_x, length_y);
		int rows, cols;
		int values[] = {1,1,1,1,1,1,1,1,1,1};
		int chance;
		for (rows =0; rows < length_y; rows++)
		{
			for (cols =0; cols < length_x; cols++)
			{
				chance =  1 ; //rand() % 10;
				ret.area[rows][cols].x_coordinate =rows;
				ret.area[rows][cols].y_coordinate =cols;
				ret.area[rows][cols].species = values[chance];
				ret.area[rows][cols].local_density = 1.0;
			}
		}
		return ret;
	}


	arraystruct local_update(unsigned long long runs)
	{
		int mcord, ncord, mcord_new, ncord_new,offset_m, offset_n, choose_non_zero,choose_zero,
			zero[3] = {1,0,-1},
			non_zero[2] = {-1,1};
//			output =0;



		unsigned long long grid_size = length_x * length_y,
						   species_counter = global_species_counter;

		arraystruct a = buildarea();


//		std::fstream one_abundance_local ("C:\\Users\\Paul\\Documents\\model_out\\timeline\\one_abundance_local.txt");
//		std::stringstream filename1;
//		filename1 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\local_" << "full" << ".txt";
//		std::ofstream local_matrix_timeline(filename1.str().c_str());


		for(unsigned long long r=0;r<runs;r++)
		{
//			unsigned long long external_species_counter_one = 0;
//			unsigned long long external_species_counter_zero = 0;
//			unsigned long long external_species_counter_two = 0;
//
//			for(int row=0;row<model::l_x;row++)
//			{
//				for(int col=0;col<model::l_y;col++)
//				{
//					if(a.area[row][col].species == 1){
//						external_species_counter_one +=1;}
//					else if(a.area[row][col].species == 0){
//						external_species_counter_zero +=1;}
//					else if(a.area[row][col].species == 2){
//						external_species_counter_two +=1;}
//				}
//			}


//				one_abundance_local << r << "," << external_species_counter_one << "," << external_species_counter_two <<","<< external_species_counter_zero << "\n";
//				std::cout << r << "," << external_species_counter_one<< "," << external_species_counter_two <<","<< external_species_counter_zero << "\n";

			int rows, cols;
			for (rows =0; rows < length_y; rows++)
			{
				for (cols =0; cols < length_x; cols++)
				{
					a.area[rows][cols].age += 1;

				}
			}
			for(unsigned long long t=0;t < grid_size; t++)
			{
//				unsigned long long internal_species_counter_one = 0;
//				unsigned long long internal_species_counter_zero = 0;
//				unsigned long long internal_species_counter_two = 0;
				mcord = function.randomInt(0,length_x-1);
				ncord = function.randomInt(0,length_y-1);

//				int rows, cols;
//				double sum_chance = 0.0;
//						for (rows =0; rows < length_y; rows++)
//						{
//							for (cols =0; cols < length_x; cols++)
//							{
//								a.area[rows][cols].dispersal = kernel(mcord, ncord, rows, cols);
//								sum_chance += a.area[rows][cols].dispersal;
//							}
//						}
//
//
//					for (rows =0; rows < length_y; rows++)
//						{
//							for (cols =0; cols < length_x; cols++)
//							{
//								a.area[rows][cols].dispersal = a.area[rows][cols].dispersal/sum_chance;
//
//							}
//						}
//


				int nullcounter = 0, runcounter = 0;
				while(nullcounter < 1 && runcounter < 4)
				{

					choose_zero = function.randomInt(0,2);
					offset_m = zero[choose_zero];

					if(offset_m == 0)
					{
						choose_non_zero = function.randomInt(0,1);
						offset_n = non_zero[choose_non_zero];
					}
					else
					{
						offset_n = 0;
					}

					mcord_new = mcord + offset_m;
					if(mcord_new < 0)
					{
						mcord_new = length_x + (mcord_new);
					}
					else if(mcord_new >= length_x)
					{
						mcord_new = mcord_new - length_x;
					}

					ncord_new = ncord + offset_n;

					if(ncord_new < 0)
					{
						ncord_new = length_y + (ncord_new);
					}
					else if(ncord_new >= length_y)
					{
						ncord_new = ncord_new - length_y;
					}


					nullcounter = a.area [mcord_new] [ncord_new].species;
					runcounter++;
				}

				a.area [mcord] [ncord].age  = 0;
				a.area [mcord] [ncord].species = a.area [mcord_new] [ncord_new].species;
//				for(int row=0;row<model::l_x;row++)
//						{
//							for(int col=0;col<model::l_y;col++)
//							{
//								if(a.area[row][col].species == 1){
//									internal_species_counter_one +=1;}
//								else if(a.area[row][col].species == 0){
//									internal_species_counter_zero +=1;}
//								else if(a.area[row][col].species == 2){
//									internal_species_counter_two +=1;}
//							}
//						}


				if(density){
					unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
													   a.area [mcord+1] [ncord].species,
													   a.area [mcord] [ncord-1].species,
													   a.area [mcord] [ncord+1].species};


					if((mcord-1) < 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) < length_y) // left boundary
					{
						unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
									 	 	 	 	 	   a.area [mcord+1] [ncord].species,
									                       a.area [mcord] [ncord-1].species,
									                       a.area [mcord] [ncord+1].species};
					}

					else if((mcord-1) >= 0 && (ncord-1) < 0 && (mcord+1) < length_x && (ncord+1) < length_y) // top boundary
					{
						unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
														a.area [mcord+1] [ncord].species,
														a.area [mcord] [length_y].species,
														a.area [mcord] [ncord+1].species};
					}

					else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) >= length_x && (ncord+1) < length_y) // right boundary
					{
						unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
														a.area [0] [ncord].species,
														a.area [mcord] [ncord-1].species,
														a.area [mcord] [ncord+1].species};
					}

					else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) >= length_y) // bottom boundary
					{
						unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
														a.area [mcord+1] [ncord].species,
														a.area [mcord] [ncord-1].species,
														a.area [mcord] [0].species};
						}

					else if((mcord-1) < 0 && (ncord-1) < 0 && (mcord+1) < length_x && (ncord+1) < length_y) // top-left boundary
					{
						unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
														a.area [mcord] [length_y].species,
														a.area [mcord+1] [ncord].species,
														a.area [mcord] [ncord+1].species};
					}

					else if((mcord-1) >= 0 && (ncord-1) < 0 && (mcord+1) >= length_x && (ncord+1) < length_y) // top-right boundary
					{
						unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
														a.area [0] [ncord].species,
														a.area [mcord] [length_y].species,
														a.area [mcord] [ncord+1].species};
					}

					else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) >= length_x && (ncord+1) >= length_y) // bottom-right boundary
					{
						unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
														a.area [0] [ncord].species,
														a.area [mcord] [ncord-1].species,
														a.area [mcord] [0].species};
					}

					else if((mcord-1) < 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) >= length_y) // bottom-left boundary
					{
						unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
														a.area [mcord+1] [ncord].species,
														a.area [mcord] [ncord-1].species,
														a.area [mcord] [0].species};
					}




					int number_of_relatives = std::count(neighbors,neighbors+4,a.area [mcord] [ncord].species);
//
				a.area[mcord][ncord].local_density = number_of_relatives/4.0;
//				double survivalrate = 0.0;

//				if(a.area [mcord] [ncord].species == 1)
//				{
//						survivalrate = 1.0 - density_strength * (internal_species_counter_one/double(256*256 - internal_species_counter_zero));
//				}
//				else if(a.area [mcord] [ncord].species == 2)
//				{
//						survivalrate = 1.0 - density_strength * (internal_species_counter_two/double(256*256 - internal_species_counter_zero));
//				}

				double survivalrate = 1.0 - density_strength * a.area[mcord][ncord].local_density;

				double survival = function.randomDouble(0.0,1.0);

				if(survival > survivalrate)
				{
					a.area [mcord] [ncord].species = 0;
				}

				}

		}
		for(int new_species = 0; new_species < speciation_rate; new_species++)
		{
			int random_x_cord = function.randomInt(0,length_x);
			int random_y_cord = function.randomInt(0,length_y);
			a.area [random_x_cord] [random_y_cord].age = 0;
			a.area [random_x_cord] [random_y_cord].species = species_counter + 1; // new species
			species_counter++;
		}



//			if( r > 50000 && r < 51001)
//			{
//					 output += 1;
//
//					std::stringstream filename1;
//					filename1 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\local_" << output << ".txt";

//					std::ofstream local_matrix_timeline(filename1.str().c_str());
//					for(int ba=0;ba<leipzigModel::l_x;ba++)
//					{
//						local_matrix_timeline << '\n';
//
//						for(int bu=0;bu<leipzigModel::l_y;bu++)
//						{
//							local_matrix_timeline << a.area[ba][bu].species <<',';
//						}
//					}

//					std::stringstream filename2;
//					filename2 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\localdensity_" << output << ".txt";
//
//					std::ofstream localdensity_matrix_timeline(filename2.str().c_str());
//					for(int ba=0;ba<model::l_x;ba++)
//					{
//						localdensity_matrix_timeline << '\n';
//
//						for(int bu=0;bu<model::l_y;bu++)
//						{
//							localdensity_matrix_timeline << a.area[ba][bu].local_density <<',';
//						}
//					}


		}
		return a;
	}


	arraystruct global_update(unsigned long long runs)
	{
		int mcord, ncord, mcord_new, ncord_new;
//		output = 0;


		unsigned long long grid_size = length_x * length_y,
						   species_counter = global_species_counter;


		arraystruct a =buildarea();

//		std::fstream one_abundance_global ("C:\\Users\\Paul\\Documents\\model_out\\timeline\\one_abundance_global.txt");
//		std::stringstream filename1;
//		filename1 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\global_" << "full" << ".txt";
//		std::ofstream global_matrix_timeline(filename1.str().c_str());

				for(unsigned long long r=1;r<runs+1;r++)
		{
//			unsigned long long external_species_counter_one = 0;
//			unsigned long long external_species_counter_zero = 0;
//			unsigned long long external_species_counter_two = 0;
//
//			for(int row=0;row<model::l_x;row++)
//			{
//				for(int col=0;col<model::l_y;col++)
//				{
//					if(a.area[row][col].species == 1){
//						external_species_counter_one +=1;}
//					else if(a.area[row][col].species == 0){
//						external_species_counter_zero +=1;}
//					else if(a.area[row][col].species == 2){
//						external_species_counter_two +=1;}
//				}
//			}
//
//			one_abundance_global << r << "," << external_species_counter_one <<","  << external_species_counter_two << "," << external_species_counter_zero <<"\n";
//			std::cout << r << "," << external_species_counter_one <<","  << external_species_counter_two << ","<< external_species_counter_zero <<"\n";
//			std::cout << r <<"\n";
			int rows, cols;
			for (rows =0; rows < length_y; rows++)
			{
			for (cols =0; cols < length_x; cols++)
			{
				a.area[rows][cols].age += 1;

			}
			}

			for(unsigned long long t=0;t < grid_size; t++)
			{
//				unsigned long long internal_species_counter_one = 0;
//				unsigned long long internal_species_counter_zero = 0;
//				unsigned long long internal_species_counter_two = 0;
				mcord = function.randomInt(0, length_x) ; //std::rand() % length_x;
				ncord = function.randomInt(0, length_y) ; //std::rand() % length_y;

				unsigned long long nullcounter = 0;
				while(nullcounter < 1)
				{
					mcord_new =  function.randomInt(0, length_x) ; // std::rand() % length_x;
					ncord_new = function.randomInt(0, length_y) ; //std::rand() % length_y;
					nullcounter = a.area [mcord_new] [ncord_new].species;
				}

				a.area [mcord] [ncord].age  = 0;
				a.area [mcord] [ncord].species = a.area [mcord_new] [ncord_new].species; //clone from existing species

//				for(int row=0;row<model::l_x;row++)
//				{
//					for(int col=0;col<model::l_y;col++)
//					{
//						if(a.area[row][col].species == 1){
//							internal_species_counter_one +=1;}
//						else if(a.area[row][col].species == 0){
//							internal_species_counter_zero +=1;}
//						else if(a.area[row][col].species == 2){
//							internal_species_counter_two +=1;}
//					}
//				}


				if(density){
					unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
													   a.area [mcord+1] [ncord].species,
													   a.area [mcord] [ncord-1].species,
													   a.area [mcord] [ncord+1].species};


						if((mcord-1) < 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) < length_y) // left boundary
						{unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
										 a.area [mcord+1] [ncord].species,
										 a.area [mcord] [ncord-1].species,
										 a.area [mcord] [ncord+1].species};
						}

						else if((mcord-1) >= 0 && (ncord-1) < 0 && (mcord+1) < length_x && (ncord+1) < length_y) // top boundary
						{unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
										 a.area [mcord+1] [ncord].species,
										 a.area [mcord] [length_y].species,
										 a.area [mcord] [ncord+1].species};
						}

						else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) >= length_x && (ncord+1) < length_y) // right boundary
						{unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
										 a.area [0] [ncord].species,
										 a.area [mcord] [ncord-1].species,
										 a.area [mcord] [ncord+1].species};
						}

						else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) >= length_y) // bottom boundary
						{unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
										 a.area [mcord+1] [ncord].species,
										 a.area [mcord] [ncord-1].species,
										 a.area [mcord] [0].species};
						}

						else if((mcord-1) < 0 && (ncord-1) < 0 && (mcord+1) < length_x && (ncord+1) < length_y) // top-left boundary
						{unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
										 a.area [mcord] [length_y].species,
										 a.area [mcord+1] [ncord].species,
										 a.area [mcord] [ncord+1].species};
						}

						else if((mcord-1) >= 0 && (ncord-1) < 0 && (mcord+1) >= length_x && (ncord+1) < length_y) // top-right boundary
						{unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
										 a.area [0] [ncord].species,
										 a.area [mcord] [length_y].species,
										 a.area [mcord] [ncord+1].species};
						}

						else if((mcord-1) >= 0 && (ncord-1) >= 0 && (mcord+1) >= length_x && (ncord+1) >= length_y) // bottom-right boundary
						{unsigned long long neighbors[4] = {a.area [mcord-1] [ncord].species,
										 a.area [0] [ncord].species,
										 a.area [mcord] [ncord-1].species,
										 a.area [mcord] [0].species};
						}

						else if((mcord-1) < 0 && (ncord-1) >= 0 && (mcord+1) < length_x && (ncord+1) >= length_y) // bottom-left boundary
						{unsigned long long neighbors[4] = {a.area [length_x] [ncord].species,
										 a.area [mcord+1] [ncord].species,
										 a.area [mcord] [ncord-1].species,
										 a.area [mcord] [0].species};
						}

					int number_of_relatives = std::count(neighbors,neighbors+4,a.area [mcord] [ncord].species);

					a.area[mcord][ncord].local_density = number_of_relatives/4.0;

//					double survivalrate = 1.0;

//				if(a.area [mcord] [ncord].species == 1)
//				{
//						survivalrate = 1.0 - density_strength * (internal_species_counter_one/double(256*256 - internal_species_counter_zero));
//				}
//				else if(a.area [mcord] [ncord].species == 2)
//				{
//						survivalrate = 1.0 - density_strength * (internal_species_counter_two/double(256*256 - internal_species_counter_zero));
//				}


					double survivalrate = 1.0 - density_strength * a.area[mcord][ncord].local_density;

					double survival = function.randomDouble(0.0,1.0);

					if(survival > survivalrate)
					{
//						std::cout << "Suvival > Survavalrate : " << survival << " : " << survivalrate << '\n';
						a.area [mcord] [ncord].species = 0;
					}

				}

			}
//			global_matrix_timeline << mcord << ';' << ncord << ';' << mcord_new << ";" << ncord_new << '\n';
	//		}
		for(int new_species = 0; new_species < speciation_rate; new_species++)
				{
					int random_x_cord = function.randomInt(0, length_x) ; // rand() % length_x;
					int random_y_cord =  function.randomInt(0, length_y) ; //rand() % length_y;
					a.area [random_x_cord] [random_y_cord].age = 0;
					a.area [random_x_cord] [random_y_cord].species = species_counter + 1; // new species
					species_counter++;
				}


//			if(r > 50000 && r % 1000 == 0)
//			{
//			std::stringstream filenamez1;
//			filenamez1 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\global_" << r << ".txt";
//
//				std::ofstream global_matrix_timeline(filenamez1.str().c_str());
//				for(int ba=0;ba<leipzigModel::l_x;ba++)
//				{
//					global_matrix_timeline << '\n';
//
//					for(int bu=0;bu<leipzigModel::l_y;bu++)
//					{
//						global_matrix_timeline << a.area[ba][bu].species <<',';
//					}
//				}
//			}
//							std::stringstream filename2;
//							filename2 << "C:\\Users\\Paul\\Documents\\model_out\\timeline\\globaldensity_" << output << ".txt";

//							std::ofstream globaldensity_matrix_timeline(filename2.str().c_str());
//							for(int ba=0;ba<model::l_x;ba++)
//							{
//								globaldensity_matrix_timeline << '\n';
//
//								for(int bu=0;bu<model::l_y;bu++)
//								{
//									globaldensity_matrix_timeline << a.area[ba][bu].local_density <<',';
//								}
//							}

		}
		return a;
	}



};


leipzigModel::leipzigModel(int x,int y, unsigned long long z, int specrate, bool dens, double densStr){ // provides a way to initialize the constructor
	length_x = x;
	length_y = y;
	speciation_rate = specrate;
	global_species_counter = z;
	density = dens;
	function.seedrand(1500);
	density_strength = densStr;
}

leipzigModel::arraystruct::arraystruct(int x, int y){
	individual area[x][y];
}
// Init static members
int leipzigModel::speciation_rate = 2;
double leipzigModel::density_strength = 0.4;
bool leipzigModel::density= false;
  

//extern "C"{
//void callLeipzig(int* x, int* y, int* dispersal, int* nSpec, int* specRate, int* runs, bool* dens, double* densityStrength, int* specOut, double* densOut){
//
//
//	leipzigModel::arraystruct mod(x[0], y[0]);
//	leipzigModel lMod(x[0],y[0],nSpec[0], specRate[0], dens[0], densityStrength[0]);
//	if(dispersal[0] == 1)	mod = lMod.global_update(runs[0]);
//	else if(dispersal[0] == 2) mod = lMod.local_update(runs[0]);
//	//	out[0] = global.area[1][1].species;
////	out[1] = global.area[1][2].species;
//	int i = 0;
//	for(int ba=0;ba<x[0];ba++){
//		for(int bu=0;bu<y[0];bu++){
//			specOut[i] = mod.area[ba][bu].species;
//			i=i+1;
//	}}
//
//	i = 0;
//	for(int ba=0;ba<x[0];ba++){
//		for(int bu=0;bu<y[0];bu++){
//			densOut[i] = mod.area[ba][bu].local_density;
//			i=i+1;
//	}}
////return out;
//}
//}

//int main()
//{
//	leipzigModel::arraystruct global;
//	leipzigModel lMod(100,100,1);
//	global = lMod.global_update(100);
//
//	std::fstream global_matrix("C:\\Users\\Paul\\Documents\\model_out\\global.txt");
//		for(int ba=0;ba<leipzigModel::l_x;ba++){
//			global_matrix << '\n';
//
//		for(int bu=0;bu<leipzigModel::l_y;bu++){
//			global_matrix << global.area[ba][bu].species <<',';
//		}}
//}

//int main()
//{
//
//	unsigned long long numberofspecies = 1;
//	leipzigModel::arraystruct global;
//	leipzigModel Model (leipzigModel::l_x,leipzigModel::l_y, numberofspecies); // initializes the model with (int * int) grid size
//	unsigned long long runs =100000;
//
//	//Running the model
//
//	global = Model.global_update(runs); // runs the update_area function (int) times
////	nearest_neighbor = Model.nearest_neighbor_update(runs);
//
//
//	// Output to .txt files
//
////	std::fstream global_matrix("C:\\Users\\Paul\\Documents\\model_out\\global.txt");
////	for(int ba=0;ba<leipzigModel::l_x;ba++){
////		global_matrix << '\n';
////
////	for(int bu=0;bu<leipzigModel::l_y;bu++){
////		global_matrix << global.area[ba][bu].species <<',';
////	}}
////
////	std::fstream nearest_neighbor_matrix("C:\\Users\\Paul\\Documents\\model_out\\nearest_neighbor.txt");
////	for(int ba=0;ba<model::l_x;ba++){
////		nearest_neighbor_matrix << '\n';
////
////	for(int bu=0;bu<model::l_y;bu++){
////		nearest_neighbor_matrix << nearest_neighbor.area[ba][bu].species <<',';
////	}}
////
////	std::fstream global_age_matrix("C:\\Users\\Paul\\Documents\\model_out\\global_age.txt");
////	for(int ba=0;ba<model::l_x;ba++){
////		global_age_matrix << '\n';
////
////	for(int bu=0;bu<model::l_y;bu++){
////		global_age_matrix << global.area[ba][bu].age <<',';
////	}}
////
////	std::fstream nearest_neighbor_age_matrix("C:\\Users\\Paul\\Documents\\model_out\\nearest_neighbor_age.txt");
////	for(int ba=0;ba<model::l_x;ba++){
////		nearest_neighbor_age_matrix << '\n';
////
////	for(int bu=0;bu<model::l_y;bu++){
////		nearest_neighbor_age_matrix << nearest_neighbor.area[ba][bu].age <<',';
////	}}
//	return 0;
//}








