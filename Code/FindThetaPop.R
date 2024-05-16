### Libraries ###
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(mixtools)
library(distr)
library(optparse)

### Set Up ###
rm(list=ls())
set.seed(420)

### Data Generating Process ###

## Parser ###
option_list = list(
  make_option(c("--Delta"), type = "numeric", default = 3, help = "Misspecification", metavar = "integer"),
  make_option(c("--Rho"), type = "numeric", default = 3, help = "Homoscedasticity", metavar = "integer"),
  make_option(c("--N"), type = "integer", default = 3, help = "Number of observations", metavar = "integer"),
  make_option(c("--Gamma"), type = "numeric", default = 3, help = "Leverage", metavar = "integer"),
  make_option(c("--K"), type = "integer", default = 3, help = "Number of covariates", metavar = "integer"),
  make_option(c("--TypeSetting"), type = "character", default = "Linear", help = "Linear vs. Logistic", metavar = "character"),
  make_option(c("--Output"), type = "character", default = NULL, help = "Path to store", metavar = "character")
)
arg.parser = OptionParser(option_list = option_list)
args = parse_args(arg.parser)

## Parameters ##
delta = args$Delta
rho = args$Rho
N = args$N
gamma = args$Gamma
K = args$K
TypeSetting = args$TypeSetting
Output = args$Output

### Set Up ###               
NSim = 100000
source(paste0("Code/SimData",TypeSetting,".R"))

pb = txtProgressBar(min = 0, 
                    max = NSim,
                    style = 3,  
                    width = 50,
                    char = "=")


### Run Simulation ###
## Set Up ##
beta_hat_list = numeric(length = NSim)

## Simulation ##
for(i in 1:NSim){
  setTxtProgressBar(pb, i)

  if(TypeSetting == "Linear"){
    SimulatedData = SimDataLinear(N = N, rho = rho, K = K, delta = delta, gamma = gamma)
  }else if(TypeSetting == "Logistic"){
    SimulatedData = SimDataLogistic(N = N, rho = rho, K = K, delta = delta, gamma = gamma)
  }
  
  dat = SimulatedData$dat
  mu = SimulatedData$mu
  
  # Model #
  if(TypeSetting == "Linear"){model = lm(Y~., data = dat)}else if(TypeSetting == "Logistic"){
    model = glm(Y~., data = dat, family = "binomial")
  }
  beta_hat = as.numeric(model$coefficients)
  epsilon_hat = as.numeric(model$residuals)
  beta_hat_list[i] = beta_hat[2]
}
close(pb)

readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds") ->test

## Save ##
beta_hat_simulation = c(delta = delta, rho = rho, N = N, gamma = gamma, K = K, BetaPop = mean(beta_hat_list))
cbind()
write.csv(beta_hat_simulation, Output)