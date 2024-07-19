/*
 * multinomial.cpp
 *
 *  Created on: 08.07.2014
 *      Author: Paul
 */

#include "Multinomial.h"

multiNom::multiNom() {

}

multiNom::~multiNom() {

}


std::default_random_engine &multiNom::my_engine() {
    static std::default_random_engine e;
    return e;
}

void multiNom::seedrand(unsigned int s) { my_engine().seed(s); }

double multiNom::randomValue(double min, double max) {
    std::uniform_real_distribution<double> unif(min, max);
    return unif(my_engine());
}


// n is the size of pvals
// return is of size N
//std::vector<int> multiNom::multinomial(int n, double *pvals)
//{
//    boost::random::mt19937 rng (time(0));
//    int sample [n];
////    sample.fill(0);
//
//    	double a = sum(pvals, n);
//		double normPvals[n];
//
//		for(int i = 0; i < (n); i++)
//		{
//			normPvals[i] = pvals[i] / a;
//		}
//
//        double remp = 1.0;
//        int rem = n, idx = 0;
//        for(int j = 0; j < n;j++)
//        {
//            double p = normPvals[j] / remp;
//            boost::random::binomial_distribution<> binom(rem, p);
//            boost::random::variate_generator<boost::random::mt19937&, boost::random::binomial_distribution<> > bpdf(rng, binom);
//            int hit = bpdf();
//            if(hit > 0)
//            {
//                sample[idx] = hit;
//            }
//            else
//            {
//            	sample[idx] = 0;
//            }
//            rem -= hit;
//            remp -= normPvals[j];
//            idx += 1;
//        }
//
//    std::vector<int> retSample;
//    retSample.assign (sample, sample + n);
//    return retSample;
//}
//
//int multiNom::getWinnerLottery(int size, double *chances)
//{
//	std::vector<int> multNom = multinomial(size, chances);
//
//	int sum = 0;
//	for(int e = 0; e < size ; e++) sum += multNom[e];
//
//	int position[sum];
//	int step = 0;
//	for(int i=0; i < size; i++)
//	{
//		for(int j=0; j < multNom[i]; j++)
//		{
//		position[step] = i;
//		step++;
//		}
//	}
//
//	int new_parent = rand() % sum;
//	return position[new_parent];
//}

int multiNom::multinomialDraw(double *chances, int size, double max) {

    double min = 0.0;

//	std::random_device rd;

//	 std::uniform_real_distribution<double> unif(min,max);
//	 std::default_random_engine re{time(0)}; // crashes why?
    double x = randomValue(min, max);
//	 std::cout << max << ':' << x << '\n';
//	 unif(re);


    bool go = true;
    int start = 0;
    int end = size;
    int mid = (start + end) / 2;
    double winner = chances[mid];
    while (go) {

        if (winner > x) {
            end = mid;
            mid = (start + end) / 2;
            winner = chances[mid];
        }

        if (winner < x) {
            start = mid;
            mid = (start + end) / 2;
            winner = chances[mid];
        }
        if (winner < x && mid == size - 1) {
            mid = size;
            winner = chances[mid];

        }


        if ((winner < x && chances[mid + 1] > x) || (winner > x && chances[mid - 1] < x) || winner == x ||
            mid == size || mid == 0) {
//			std::cout << "go" << ':' << size << ":" << winner << ":" << x << ":" << start<< ":" << mid << ":" << end << '\n';
            go = false;
        }


    }

    return mid;

}

