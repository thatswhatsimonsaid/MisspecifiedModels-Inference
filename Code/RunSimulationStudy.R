# Set Up
## Libr.
{r message=FALSE, warning=FALSE}
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(mixtools)
library(distr)


## Rm. Var.
rm(list=ls())
dir = "/Users/simondn/Documents/Stats572/"


# Functions
source("Code/functions/AIZResults.R")
source("Code/functions/ConfidenceIntervalFunction.R")
source("Code/functions/OneIterationFunction.R")
source("Code/functions/SimDataLinear.R")
source("Code/functions/SimDataLogistic.R")
source("Code/functions/SimulationFunction.R")
source("Code/functions/SimulationReformatResultsFunction.R")
source("Code/functions/ThetaCondFunction.R")
source("Code/functions/ThetaPopFunction.R")
source("Code/functions/VHatCondFunction.R")
source("Code/functions/VHatPopFunction.R")
source("Code/functions/WhichMinFunction.R")


# Set Up

### Set Up ###
set.seed(420)
TypeSetting = "Linear"
ParameterVector = cbind(MisspecVec = c(rep(0,16),rep(1,16)),                        # Delta: Misspecification Rate
                        HomoskedVec = rep(c(rep(0,8), rep(0.5,8)),2),                # Gamma: Heteroskedasticity Rate
                        SizeVec = rep(c(rep(50,4), rep(200,4)),4),                   # N: Observations
                        LeverageVec = rep(c(0,0,0.1,0.1),8),                         # rho: Mixture Leverage
                        KVec = rep(c(1,5),16)                                        # K: Parameters
                        ) %>% data.frame


# Simulation

### Run Simulation ###
NSim = 10
SimulationResultsList = list(length = nrow(ParameterVector))
for(i in 1:nrow(ParameterVector)){
  print(paste0("Case ",i))
SimulationResultsList[[i]] = SimulationFunction(NSim = NSim, 
                                                ParameterVector = ParameterVector, 
                                                SimulationCase = i, 
                                                VarFixed = NA,
                                                TypeSetting = TypeSetting)
  # write.csv(SimulationResultsList[[i]]$SimulationSEResults, 
            # file =  "data/SimulationCases/",TypeSetting,"/StandardErrors/Case",i,".csv"))
  # write.csv(SimulationResultsList[[i]]$SimCoverageFrequency, 
            # file =  "data/SimulationCases/",TypeSetting,"/Coverage/Case",i,".csv"))
}
names(SimulationResultsList) = paste0("Case",1:nrow(ParameterVector))
# saveRDS(SimulationResultsList, file = "data/SimulationCases/",TypeSetting,"/SimulationResults.rds"))





