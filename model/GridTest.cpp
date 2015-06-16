/*
 * gridtest.cpp
 *
 *  Created on: 24.06.2014
 *      Author: Paul
 */


#include "grid.h"
#include<iostream>

int main(){
	grid Grid(256,256);
	std::cout << Grid.get_dimensions().first << Grid.get_dimensions().second;

	return 0;
}

