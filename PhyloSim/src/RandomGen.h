/*
 * RandomGen.h
 *
 *  Created on: 10.07.2014
 *      Author: Paul
 *              Betim Musa <musab@informatik.uni-freiburg.de>
 */

#ifndef MULTINOMIAL_H_
#define MULTINOMIAL_H_

#include <random>
#include <vector>


/*! \class Test
 *  \brief A short description.
 *
 *  More text.
 */
class RandomGen {
 public:
	RandomGen();
	~RandomGen();
	std::vector<int> multinomial(int n, double *pvals);
	int getWinnerLottery(int size, double *chances);
	int multinomialDraw(double *chances, int size, double max);
	std::default_random_engine & my_engine();
	void seedrand(unsigned int s);
	double randomDouble(double min, double max);
	int randomInt(int min, int max);
	int randomPoisson(double mean);
};

extern "C" {

int callMultinomialDraw(double *chances, int size, double max, double* out);

int callRandomGenRandomPoisson(double* x, double* out);
}

#endif /* MULTINOMIAL_H_ */
