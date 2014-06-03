rm(list=ls())
setwd("~/Desktop/UW/Spring 2014/AMATH 543 Portfolio/HW 4/")
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# TO is the function to calculte the Turn Over at balance dates
# Input: a zoo object containing the weights at balance dates
# Output: a zoo object containing the turn over at the balance dates

TO <- function(weights){
  dates <- index(weights)
  weights <- coredata(weights)
  n.asset <- ncol(weights)
  n.dates <- nrow(weights)
  if(n.dates < 2){
    print("Less than 2 balancing dates!")
    return()
  }
  TurnOver <- rep(0, n.dates-1)
  for(i in 1:length(TurnOver)){
    TurnOver[i] <- sum(abs(weights[i+1,]-weights[i,]))
  }
  dates <- dates[-1]
  res <- zoo(TurnOver, order.by = dates)
  print(index(res))
  res
}

# DIV is the function to calculate the diversification
# Input: a zoo object containing the weights at balance dates
# Output: a zoo object containing the DIV values at the balance dates

DIV <- function(weights){
  n.dates <- nrow(weights)
  if(n.dates<1){
    print("empty data set")
    return()
  }
  diversification <- rep(0,n.dates)
  for(i in 1:n.dates){
    diversification[i] <- 1-sum(weights[i,]^2)
  }
  dates <- index(weights)
  Div <- zoo(diversification,dates)
  return(Div)
}

ret.weekly = read.zoo("smallcap.csv",sep=",",header = T, format = "%m/%d/%Y")
as.xts(ret.weekly)
RETURNS <- ret.weekly[,1:20]
MARKET <- ret.weekly[,"VWMKT"]
funds <- colnames(RETURNS)

pspec <- portfolio.spec(assets=funds)
PP.a <- PP.b <- PP.c <- add.constraint(pspec, type="full_investment")
PP.aLO <- add.constraint(PP.a, type = "long_only")
PP.aLO <- add.objective(PP.aLO, type = "risk", name = "var")
PP.bB <- add.constraint(PP.b, type = "box", min = 0, max = 0.20)
PP.bB <- add.objective(PP.bB, type = "risk", name = "var")
PP.cBS <- add.constraint(PP.c, type = "box", min = -0.10, max = 0.30)
PP.cBS <- add.objective(PP.cBS, type = "risk", name = "var")

# 1a
# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.LO <- optimize.portfolio.rebalancing(RETURNS, PP.aLO,
                                        optimize_method="ROI",
                                        rebalance_on="months",
                                        training_period=60)

# Extract time series of portfolio weights
wts.LO <- extractWeights(bt.LO)
TO.LO <- TO(wts.LO)
DIV.LO <- DIV(wts.LO)
print(paste("Long Only: Mean Turnover: ", mean(TO.LO)))
print(paste("Long Only: Mean Diversification: ", mean(DIV.LO)))

png(file <- "AC_HW4_1aTO.png", width = 8, height = 5, units = "in",res = 300)
plot(TO.LO, main = paste("Time Series Turn Over, Mean: ", round(mean(TO.LO),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Turn Over")
dev.off()

png(file <- "AC_HW4_1aDIV.png", width = 8, height = 5, units = "in",res = 300)
plot(DIV.LO, main = paste("Time Series Diversification, Mean: ", round(mean(DIV.LO),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Diversification")
dev.off()

# Compute cumulative returns of portfolio
GMV.LO <- Return.rebalancing(RETURNS, wts.LO)
index(GMV.LO) <- as.Date(index(GMV.LO))

# Combine GMV.LO and MARKET cumulative returns
ret.comb <- na.omit(merge(GMV.LO, MARKET, all=F))

# return analysis
png(file <- "AC_HW4_1a.png", width = 8, height = 5, units = "in",res = 300)
charts.PerformanceSummary(ret.comb, wealth.index = T,
                          lty = c(1,3,2), colorset = c("black","red","blue"),
                          cex.legend = 1.3, cex.axis = 1.3)
dev.off()

# 1b
# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.B <- optimize.portfolio.rebalancing(RETURNS, PP.bB,
                                        optimize_method="ROI",
                                        rebalance_on="months",
                                        training_period=60)

# Extract time series of portfolio weights
wts.B <- extractWeights(bt.B)
TO.B <- TO(wts.B)
DIV.B <- DIV(wts.B)
print(paste("Long Only: Mean Turnover: ", mean(TO.B)))
print(paste("Long Only: Mean Diversification: ", mean(DIV.B)))

png(file <- "AC_HW4_1bTO.png", width = 8, height = 5, units = "in",res = 300)
plot(TO.B, main = paste("Time Series Turn Over, Mean: ", round(mean(TO.B),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Turn Over")
dev.off()

png(file <- "AC_HW4_1bDIV.png", width = 8, height = 5, units = "in",res = 300)
plot(DIV.B, main = paste("Time Series Diversification, Mean: ", round(mean(DIV.B),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Diversification")
dev.off()

# Compute cumulative returns of portfolio
GMV.B <- Return.rebalancing(RETURNS, wts.B)
index(GMV.B) <- as.Date(index(GMV.B))

# Combine GMV.LO and MARKET cumulative returns
ret.comb <- na.omit(merge(GMV.B, MARKET, all=F))

# return analysis
png(file <- "AC_HW4_1b.png", width = 8, height = 5, units = "in",res = 300)
charts.PerformanceSummary(ret.comb, wealth.index = T,
                          lty = c(1,3,2), colorset = c("black","red","blue"),
                          cex.legend = 1.3,cex.axis = 1.3)
dev.off()

# 1c
# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.BS <- optimize.portfolio.rebalancing(RETURNS, PP.cBS,
                                       optimize_method="ROI",
                                       rebalance_on="months",
                                       training_period=60)

# Extract time series of portfolio weights
wts.BS <- extractWeights(bt.BS)
TO.BS <- TO(wts.BS)
DIV.BS <- DIV(wts.BS)
print(paste("Long Only: Mean Turnover: ", mean(TO.BS)))
print(paste("Long Only: Mean Diversification: ", mean(DIV.BS)))

png(file <- "AC_HW4_1cTO.png", width = 8, height = 5, units = "in",res = 300)
plot(TO.BS, main = paste("Time Series Turn Over, Mean: ", round(mean(TO.BS),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Turn Over")
dev.off()

png(file <- "AC_HW4_1cDIV.png", width = 8, height = 5, units = "in",res = 300)
plot(DIV.BS, main = paste("Time Series Diversification, Mean: ", round(mean(DIV.BS),5)), 
     col = "dodgerblue", xlab = "Year", ylab = "Diversification")
dev.off()

# Compute cumulative returns of portfolio
GMV.BS <- Return.rebalancing(RETURNS, wts.BS)
index(GMV.BS) <- as.Date(index(GMV.BS))

# Combine GMV.LO and MARKET cumulative returns
ret.comb <- na.omit(merge(GMV.BS, MARKET, all=F))

# return analysis
png(file <- "AC_HW4_1c.png", width = 8, height = 5, units = "in",res = 300)
charts.PerformanceSummary(ret.comb, wealth.index = T,
                          lty = c(1,3,2), colorset = c("black","red","blue"),
                          cex.legend = 1.3,cex.axis = 1.3)
dev.off()