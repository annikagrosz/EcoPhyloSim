/*
 * multinomial.h
 *
 *  Created on: 10.07.2014
 *      Author: Paul
 */

#ifndef MULTINOMIAL_H_
#define MULTINOMIAL_H_

#include <random>
#include <vector>

class multiNom {
public:
    multiNom();

    ~multiNom();

    std::vector<int> multinomial(int n, double *pvals);

    int getWinnerLottery(int size, double *chances);

    int multinomialDraw(double *chances, int size, double max);

    std::default_random_engine &my_engine();

    void seedrand(unsigned int s);

    double randomValue(double min, double max);
};


#endif /* MULTINOMIAL_H_ */
