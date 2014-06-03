# Andrew Coogan
# Homework 4
# Problem 1

rm(list=ls())

###
# a
###

S0 <- 100
K <- 125
r <- 0.05
d <- 0.02
vol <- 0.30
TTM <- 1
paths <- 1e5

time.steps <- 1
dt <- TTM/time.steps

d.1 <- ( log(S0/K) + (r-d+vol^2/2) ) / vol
d.2 <- d.1 - vol
AP <- K*exp(-r*TTM)*pnorm(-d.2) - S0*exp(-d*TTM)*pnorm(-d.1)

exp1 <- (r - d - vol^2/2)*dt
exp2 <- vol * sqrt(dt)

# CRUDE MONTE CARLO
CMC <- rep(S0, paths)
for(i in 1:time.steps)
{
  eps <- rnorm(paths)
  CMC <- CMC*exp(exp1 + exp2*eps)
}
CMC.payoff <- exp(-r*TTM)*pmax(K-CMC,0)
CMC.price <- mean(CMC.payoff)
CMC.se <- sd(CMC.payoff)/sqrt(paths)
CMC.ep <- 100*abs(CMC.price-AP)/AP

# ANTITHETIC VARIATE
AV.1 <- AV.2 <- rep(S0, paths/2)
for(i in 1:time.steps)
{
  eps <- rnorm(paths/2)
  AV.1 <- AV.1*exp(exp1 + exp2*eps)
  AV.2 <- AV.2*exp(exp1 - exp2*eps)
}
AV.payoff.1 <- pmax(K-AV.1,0)
AV.payoff.2 <- pmax(K-AV.2,0)
AV.payoff <- 0.5*(AV.payoff.1 + AV.payoff.2)
AV.price <- exp(-r*TTM)*mean(AV.payoff)
AV.se <- sd(AV.payoff)/sqrt(paths/2)
AV.ep <- 100*abs(AV.price-AP)/AP

# CONTROL VARIATE
CV <- rep(S0, paths)
for(i in 1:time.steps)
{
  eps <- rnorm(paths)
  CV <- CV*exp(exp1 + exp2*eps)
}
CV.payoff <- exp(-r*TTM)*pmax(K-CV,0)
beta <- cov(CV.payoff, CV)/var(CV)
CV.payoff.prime <- CV.payoff - beta*(CV - (S0*exp(r-d)))
CV.price <- (1/paths) * sum(CV.payoff.prime)
CV.se <- sd(CV.payoff.prime)/sqrt(paths)
CV.ep <- 100*abs(CV.price-AP)/AP

c.names <- c("Crude Monte Carlo", "Antithetic Variates", "Control Variates")
r.names <- c("Price", "Standard Error", "Percent Error")
data <-matrix(c(CMC.price,CMC.se,CMC.ep,AV.price,AV.se,AV.ep,CV.price,CV.se,CV.ep),nrow = 3)
row.names(data) <- r.names
colnames(data) <- c.names
print(data)

###
# b
###

pilot.simulations <- 2e3
total.simulations <- 1e5
M <- 40
p <- 1/M
strata.pct <- seq(from = 0, to = 1, length.out = M+1)

Z.strat <- function(a, b, n.sim)
{
  qnorm(a + (b - a)*runif(n.sim))
}

var <- rep(0,length(strata.pct)-1)

for(i in 1:length(var))
{
  Z <- Z.strat(strata.pct[i], strata.pct[i+1], pilot.simulations)
  pilot.values <- exp(-r*TTM)*pmax(K - S0*exp(exp1 + exp2*Z), 0)
  var[i] <- var(pilot.values)
}

std <- sqrt(var)
n <- round(total.simulations*p*std/(sum(p*std)))

sigma.hat <- mu.hat <- 0
for(i in 1:length(n))
{
  if(n[i] > 0)
  {
    sum <- sum.squ <- tV <- 0
    Z <- Z.strat(strata.pct[i], strata.pct[i+1], n[i])
    tV <- exp(-r*TTM)*pmax(K - S0*exp(exp1 + exp2*Z), 0)
    sum <- sum(tV)
    sum.squ <- sum(tV^2)
    mu.hat <- mu.hat + (p*sum/n[i])
    sig.i <- (sum.squ - sum^2/n[i])/(n[i]-1)
    sigma.hat <- sigma.hat + p*p*sig.i/n[i]
  }
}

ST.price <- mu.hat
ST.se <- sqrt(sigma.hat)
ST.ep <- 100*abs(ST.price-AP)/AP

c.name <- c("Stratified Method")
r.name <- c("Price", "Standard Error", "Percent Error")
data.ST <-matrix(c(ST.price,ST.se,ST.ep),nrow = 3)
row.names(data.ST) <- r.name
colnames(data.ST) <- c.name
print(data.ST)