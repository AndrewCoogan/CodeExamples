// This is an automated trading program that I created using factor analysis.

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/variance.hpp>
#include <boost/bind.hpp>
#include <boost/ref.hpp>

using namespace std;
using namespace boost::accumulators;

struct pPositions
{
    double movingValue;
    //This is the total value of the portfolio.  It is going to be updated at each iteration when the
    //  portfolio is converted to cash, before reinvesting it in the following function.
    
    vector<double> mReturns;
    //This is going to take account the percentage move per month and then certain statistics will
    //  take place on this array such as mean and variance.
    
    //vector<int> indexNumbers;
    //PLAN:  Use these to keep track, so there is no OLD and NEW index vectors in the main program,
    //  just vIndex.  We can alter that completely and keep indexNumbers as the current position numbers.
    
    vector<double> SHARES;  //This will be the number of shares produced.
    // NOTE:  This function runs off of standard assumptions, namely, all assets are infinitely divisible
    //  This may mean that there are 3.5 shares in a company or something similar.
};

int findMaxDate(vector<int> dates)
{
    // This is just a function to find the max date in the series.  As in, the last month to sell all the
    //  assets in the portfolio and end the trading loop with an all cash portfolio.
    
    int temp = dates[0];
    for(int i = 1; i < dates.size(); i++)
    {
        if(temp < dates[i])
        {
            temp = dates[i];
        }
    }
    return temp;
}

vector<int> getAllDates(int startDate, int endDate)
{
    // This function finds all the possible dates in the data range, and returns everything in the
    //  proper format.  It starts with the first date you want to start trading on, and ends including
    //  the final date.
    
    vector<int> allDates;
    allDates.push_back(startDate);
    
    while(allDates.back() < endDate)
    {
        if( (allDates.back()%100) != 12 )
        {
            allDates.push_back(allDates.back() + 1);
        }
        else if( (allDates.back()%100) == 12 )
        {
            allDates.push_back(allDates.back() + 89);
        }
    }
    
    return allDates;
}

vector<int> findDates(vector<int> vDates, int desiredDate, unsigned long vL)
{
    // This function returns a vector of index numbers that have the same date as the one
    //  you are looking for.  ie, you want to trade on 200101 (January 2001), this function will
    //  return a vector with all the index numbers of every asset with information on that date.
    
    vector<int> dateIndex;
    for(int i = 0; i < vL; i++)
    {
        if(vDates[i] == desiredDate)
        {
            dateIndex.push_back(i);
        }
    }
    return dateIndex;
}

vector<int> checkActive(vector<int> vIndex, vector<int> vActive)
{
    // The previous function (findDates), returns an index vector with every asset with information on
    //  that date, but not all assets are active.  This function checks that the asset is active.  If it
    //  is not active, then we dont want to trade it.  This function takes in a vector of length N, and
    //  outputs a vector of length less than or equal to N.
    
    vector<int> dIndex;
    unsigned long COUNT = vIndex.size();
    
    for(int i = 0; i < COUNT; i++)
    {
        if(vActive[vIndex[i]] == 1)
        {
            dIndex.push_back(vIndex[i]);
        }
    }
    return dIndex;
}

vector<int> checkIndex(vector<int> vIndex, vector<long> volume)
{
    // This is similar to the checkActive function, but has more potential.  As it stands, this function
    //  checks that the volume is actually positive and that the company is being traded and not bankrupt.
    
    vector<int> output;
    
    for(int i = 0; i < vIndex.size(); i++)
    {
        if( (volume[vIndex[i]] != -2) && (volume[vIndex[i]] != 0) )
        {
            // If you want to add more no-trade constraints, just do them here.
            // Add as many if-statements you want and include it into the input parameters.
            output.push_back(vIndex[i]);
        }
    }
    
    return output;
}

vector<int> nRank(vector<int> vIndex, vector<double> vConstraint, int nA, bool LOW)
{
    // The overall idea for this function is to find the highest or lowest values in the constraint
    //  vector with indices passed in through vIndex, and return those optimal values.
    
    // This tests that there is an adequate number of index vectors from the data to create a a fully
    //  invested portfolio per the number of desired assets in the portfolio.
    
    if(nA > vIndex.size())
    {
        cout << "Your portfolio has too many assets and will not produce reliable results." << endl;
        cout << "Lower the number of assets and try again." << endl;
    }
    
    vector<int> output;
    vector<double> aE;
    
    //This vector will store all elements fro vConstraint with index number found in vIndex.
    
    for(int i = 0; i < vIndex.size(); i++)
    {
        aE.push_back(vConstraint[vIndex[i]]);
    }
    
    sort(aE.begin(), aE.end());  //Returns aE sorted from smallest to largest
    if(!LOW) {reverse(aE.begin(), aE.end());}  //If activated, will find lowest values rather than largest
    
    
    for(int j = 0; j < nA; j++)  //This will find the best overall nA assets.
    {
        for(int k = 0; k < vConstraint.size(); k++)
        //Will cycle through until it finds the proper value, then break as to save time.
        {
            if(aE[j] == vConstraint[k])
            {
                output.push_back(k);
                break;
            }
        }
    }
    
    return output;
    //This now returns a vector of indices that have the highest (or lowest) indices that both satisfy
    //  the date, active, and volume constraint, among any other constraint established using nRank.
}

pPositions enterPositions(vector<int> vIndex, vector<double> w, pPositions PP, vector<double> price)
{
    // This function takes in the indexes we want to invest in (vIndex), the weights of how much of
    //  the portfolio to invest in (weights), and the prices of the assets (current share price) (price).
    //  The amount of shares will be saved in the PP.SHARES vector list, and vIndex will be unaltered
    //  to keep what we invested in.  There is also a vector in pPostions structure to save that if
    //  that is something that we want to add later.
    
    double newShares = 0;  //The number of shares that would allow us to invest w[i]*PortfolioValue
    
    for(int i = 0; i < vIndex.size(); i++)
    {
        newShares = w[i]*PP.movingValue/price[vIndex[i]];  //Transaction costs go here.
        PP.SHARES.push_back(newShares);
    }
    return PP; //This is so there is only one portfolio being used in the program and it is contained.
}

pPositions clearPositions(vector<int> vIndex, pPositions PP, vector<double> price, vector<double> div,
                          int date)
{
    // This function clears all of the positions and sells everything after one month of holding time.
    //  Takes into account dividend payments, does not take into account transaction costs or the time
    //  loss of money.  After 20 years, with a risk free rate of 0.04 , we are looking at an overall
    //  discount factor of ~0.45 at the end of the program.
    
    double cValue = 0;
    double iValue = PP.movingValue;
    double mRet = 0;
    
    //vIndex is increased in the main function, so it is incorrect to do it here.
    for(int i = 0; i < vIndex.size(); i++)
    {
        cValue += PP.SHARES[i] * (price[vIndex[i]] + div[vIndex[i]]); //Transaction costs go here.
    }
    
    PP.movingValue = cValue;  //Sets the portfolio to the new value
    PP.SHARES.clear();  //Clears the shares so that the vector can be constructed in enterPositions.
    
    mRet = 100*((cValue/iValue) - 1);  //Monthly return
    
    PP.mReturns.push_back(mRet/100);  //saving the monthly return for boost analysis later
    
    cout << "Monthly Return for (" << date << ") is: " << mRet <<  "%" << endl;  //Output to follow.
    
    return PP;
    // Once again, we only want one portfolio in the program, so we return te portfolio we entered with
    //  modified information.
}


int main()
{
    // MANUAL INPUTS
    int DATE = 199312; // YYYYMM, start date, can be arbitrary
    int nAssets = 100; // We want to invest in 5 assets
    double portfolioValue = 1000000;
    // END MANUAL INPUTS

    double weightsEQ = 1/(double)nAssets;  // This calculates the weights for using equal weights
    vector<double> weights(nAssets, weightsEQ); // This is assuming equal weights, fully invested

    
    //Information in Order
    vector<int> date;
    vector<int> id;
    vector<string> ticker;
    vector<int> active;
    vector<double> pSTART;
    vector<double> VWAP;
    vector<double> pCLOSE;
    vector<double> pEND;
    vector<long> volume;
    vector<double> div;
    // Ignore short vol
    // Ignore short rate
    vector<double> EY; // Earnings Yield
    vector<double> ROE;  // Return on Equity
    vector<double> ETA;  // Equity to Asset
    vector<double> EPSC; // Earnings per share growth consistency
    
    vector<int> iIndex; // Initial portfolio positions, Final portfolio positions
    pPositions portfolioPositions;  // This is the portfolio that we will use the entire program
    accumulator_set<double, features<tag::mean, tag::variance> > portfolioStats;  // Set up the accumulator
    
    string row;  // Used solely for "pulling the data out of the text file
    int iVal;  // Used solely for "pulling the data out of the text file
    
    string fileLocation = "/Users/drewcoogan/Desktop/UW/Summer 2014/Euclidean Technologies/data2.txt";
    // You are going to want to change this when running on different machines.
    
    fstream inputFile(fileLocation, ios::in);
    
    if(!inputFile)
    {
        cout << "There has been an error importing the data file." << endl;
    } else
    {
        while( getline(inputFile, row) )
        {
            istringstream iss(row);
            
            iVal = row.find(" ");
            date.push_back(atoi(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            id.push_back(atoi(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            ticker.push_back(row.substr(0, row.find(" ")));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            active.push_back(atoi(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            pSTART.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            VWAP.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            pCLOSE.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            pEND.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            volume.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            div.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" "); row.erase(0, iVal + 1); //Ignoring Short Volume
            iVal = row.find(" "); row.erase(0, iVal + 1); //Ignoring Short Rate
            
            iVal = row.find(" ");
            EY.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            ROE.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            ETA.push_back(atof(&row.substr(0, row.find(" "))[0]));
            row.erase(0, iVal + 1);
            
            iVal = row.find(" ");
            EPSC.push_back(atof(&row.substr(0, row.find(" "))[0]));
            
            iss.clear();
        }
    }
    
    int maxDate = findMaxDate(date);
    // Finds the max date in the
    vector<int> allDates = getAllDates(DATE, maxDate);
    // This function finds all dates within .txt file starting with the one specified line 234.
    
    portfolioPositions.movingValue = portfolioValue;  // Initializes the value of the portfolio
    
    for(int i = 0; i < (allDates.size()-1); i++)
    {
        iIndex = findDates(date, allDates[i], date.size()); // Finds all dates with given parent date
        iIndex = checkActive(iIndex, active);  // Are all the assets with the date active?
        iIndex = checkIndex(iIndex, volume);  // We can add more values if we find something good

        iIndex = nRank(iIndex, ROE, 3*nAssets, false);
        // First take the top 300 ROE for assets active, and save those index numbers.
        
        iIndex = nRank(iIndex, EY, nAssets, false);
        // Then pass that along to another search function to find the top 100 EY out of the top 300 ROE.
        
        portfolioPositions = enterPositions(iIndex, weights, portfolioPositions, pSTART);
        // Enter into the positions given the "optimal" index numbers in iIndex
        
        transform(iIndex.begin(), iIndex.end(), iIndex.begin(), bind2nd(std::plus<double>(), 1.0));
        // This adds one to iIndex, ie, moves the date up 1.0 so that it sells at the following month.
        
        portfolioPositions = clearPositions(iIndex, portfolioPositions, pSTART, div, allDates[i+1]);
        //This looks at time+1 and clears the portfolio before it it incremented next iteration.
        //The index is cleared below so that there is no data conflict.
        
        iIndex.clear();
        // Makes sure there are no conflicting orders or index numbers.  At this point out portfolio is
        //  clear, and it does not matter what we invested in last month, so we can clear it out.
    }
    
    // The rest of the program is outputting the final data.  There is no correction for the time value
    //  of money.  As commented above, for 20 years at 4%, we are looking at about a 50% discount factor.
    
    cout << "The value of the portfolio is now: " << portfolioPositions.movingValue << endl;
    
    cout << "Total Portfolio return, using monthly rebalancing, since starting algorithm: " <<
    ((portfolioPositions.movingValue/portfolioValue) - 1)*100 << "%" << endl;
    
    // This puts the values found in mReturns from the portfolio structure into an accumulator.
    //  We are going to do some statistics on this.  These are some basic statistical measures,
    //  there are some more advanced and interesting measurements found in the boost library.
    for_each(portfolioPositions.mReturns.begin(), portfolioPositions.mReturns.end(),
             boost::bind<void>( ref(portfolioStats), _1) );
    
    cout << "Average Monthly Return: " << extract::mean(portfolioStats) << endl;
    // Average Monthly Return
    
    cout << "Monthly Return Volatility: " << sqrt(extract::variance(portfolioStats)) << endl;
    // Average Monthly Volatility
    return 0;
}