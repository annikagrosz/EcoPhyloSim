/*
 * multinomial.cpp
 *
 *  Created on: 08.07.2014
 *      Author: Paul
 */

#include "RandomGen.h"

RandomGen::RandomGen(){

}
RandomGen::~RandomGen(){

}

std::default_random_engine & RandomGen::my_engine( )
{
 static std::default_random_engine e;
 return e;
}

void RandomGen::seedrand(unsigned int s) { my_engine().seed(s); }

double RandomGen::randomDouble(double min, double max)
{
	 std::uniform_real_distribution<double> unif(min,max);
	 return unif(my_engine());
}


int RandomGen::randomInt(int min, int max){
	std::uniform_int_distribution<int>unif(min, max);
	return unif(my_engine());
}

int RandomGen::randomPoisson(double mean){
std::poisson_distribution<int>pois(mean);
return pois(my_engine());
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


// Chances need to be a cummulative function, unnormalized
// Would like to check this again in detail

int RandomGen::multinomialDraw(double * chances, int size, double max)
{

	double min = 0.0;
	double x = randomDouble(min, max);

	bool go = true;
	int start = 0;
	int end = size;
	int mid = size/2;
	double midValue = chances[mid];
	while(go)
	{
		if(x < midValue)
		{
			end = mid;
			mid = (start+end)/2;
			midValue = chances[mid];
		}else{
			start = mid;
			mid = (start+end)/2;
			midValue = chances[mid];
		}

		//if (mid == 0) return mid;
		if (start == mid){
			if (mid == 0 && x < midValue ) return 0;
			else return mid + 1;
		}




//		//if((midValue < x && chances[mid+1] > x) || (midValue > x && chances[mid-1] < x) || midValue == x || mid == size || mid == 0)
//		{
////			std::cout << "go" << ':' << size << ":" << winner << ":" << x << ":" << start<< ":" << mid << ":" << end << '\n';
//			go = false;
//		}
	}
	return 1;
}


int callRandomGenRandomPoisson(double* x, double* out )
{
	RandomGen r;
	out[0] = r.randomPoisson(x[0]);
	return out[0];
}


int callMultinomialDraw(double *chances, int size, double max, int* out)
{
	RandomGen r;
	out[0] = r.multinomialDraw(chances, size, max);
	return out[0];
}
