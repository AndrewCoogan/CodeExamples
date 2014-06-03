#include <iostream>
#include <cmath>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>

using namespace std;
using namespace boost::accumulators;

double priceCall(double S0, double K, double r, double div, double vol, double TTM, int N, int M)
{
    /*
     S0 : Initial stock price of an asset
     K : Strike price of the call option
     r : continuously compounded interest rate
     div : continuously paid dividend payments
     vol : volatility of the asset
     TTM : time to maturity
     N : number of time steps
     M : number of simulations
     */
    
    boost::mt19937 rng;
    boost::normal_distribution<> nd(0.0, 1.0);
    boost::variate_generator< boost::mt19937&, boost::normal_distribution<> > eps(rng, nd);
    rng.seed(static_cast<unsigned int>(std::time(0)));

    
    accumulator_set<double, stats<tag::mean>> c_out;
    
    double price = 0.0;
    double asset_price = 0.0;
    double payoff = 0.0;
    double dt = TTM/N;
    double exp_1 = (r - div - (vol*vol/2))*dt;
    double exp_2 = vol*sqrt(dt);
    
    clock_t startTime = clock();
    
    for(int i = 0; i < M; i++)
    {
        asset_price = S0;
        for(int j = 0; j < N; j++) { asset_price *= exp(exp_1 + (exp_2*eps())); }
        payoff = fmax(asset_price-K, 0);
        c_out(payoff);
    }
    price = mean(c_out) * exp(-r*TTM);
    
    cout << "Run time: " << double(clock() - startTime) / (double)CLOCKS_PER_SEC << " seconds." << endl;
    
    return price;
}

int main()
{
    double prem = 0.0;
    prem = priceCall(100, 100, 0.06, 0.03, 0.20, 1, 252, 1e6);
    cout << "Price of the call is: $" << prem << endl;
    return 0;
}