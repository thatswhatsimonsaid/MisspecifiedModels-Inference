### Summary:
# A script to run the simulation in the high-performance computing cluster
### Inputs:
# Delta: Misspecification rate from the command line.
# Rho: Leverage rate from the command line.
# N: Number of observations from the command line.
# Gamma: Heteroscedasticity rate from the command line.
# K: Number of covariates from the command line.
# TypeSetting: Linear/Logistic from the command line.
# NSim: Number of simulations.
### Output:
# beta_hat_simulation: A .rds file containing the results of the function
#                      SimulationFunction.


# Set Up
## Libr.
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(mixtools)
library(distr)
library(optparse)

## Rm. Var.
rm(list=ls())

## Functions
source("Code/AIZResults.R")
source("Code/ConfidenceIntervalFunction.R")
source("Code/OneIterationFunction.R")
source("Code/SimDataLinear.R")
source("Code/SimDataLogistic.R")
source("Code/SimulationFunction.R")
source("Code/SimulationReformatResultsFunction.R")
source("Code/ThetaCondFunction.R")
source("Code/ThetaPopFunction.R")
source("Code/VHatCondFunction.R")
source("Code/VHatPopFunction.R")
source("Code/WhichMinFunction.R")


## Parser ###
option_list = list(
  make_option(c("--Delta"), type = "numeric", default = 3, help = "Misspecification", metavar = "integer"),
  make_option(c("--Gamma"), type = "numeric", default = 3, help = "Homoscedasticity", metavar = "integer"),
  make_option(c("--N"), type = "integer", default = 3, help = "Number of observations", metavar = "integer"),
  make_option(c("--Rho"), type = "numeric", default = 3, help = "Leverage", metavar = "integer"),
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


### Simulation ###
set.seed(1)
NSim = 10000

SimulationFunction(NSim = NSim, 
                   delta = delta,
                   gamma = gamma,
                   N = N,
                   rho = rho,
                   K = K,
                   VarFixed = NA,
                   TypeSetting = TypeSetting) -> SimulationResults

saveRDS(SimulationResults,Output)




