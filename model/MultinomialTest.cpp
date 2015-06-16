/*
 * multinomtest.cpp
 *
 *  Created on: 21.07.2014
 *      Author: Paul
 */

#include "randomGen.h"
int main()
{
	randomGen mult;
	mult.seedrand(1500);
	double chances[] = {0.1,0.3,0.099999, 0.000001,0.2,0.3};
	int length = 6;
	double cumChances[length];
	cumChances[0] = chances[0];
	for(int i=1; i < length; i++)
	{
		cumChances[i] = cumChances[i-1] + chances[i];
	}
	double hits [length];

	for(int i=0; i < length; i++)
	{
		hits[i] = 0;
	}

	int total = 10000000;

	for(int i=0; i< total; i++)
	{
//		int x = mult.getWinnerLottery(15,chances);
		int x = mult.multinomialDraw(cumChances, length-1, cumChances[length-1]);
		for (int j = 0; j < length; j++)
		{
			if( x == j)
			{
				hits[j]++;
			}
		}
	}

	for(int i=0; i < length; i++)
	{
		hits[i] = hits[i]/(double)total;
	}


	for (int k = 0; k< length; k++){
	std::cout << "chance : " << chances[k] << " hits : " << hits[k] << '\n';
	std::cout << cumChances[k] << '\n';
	}
	return 0;
}

