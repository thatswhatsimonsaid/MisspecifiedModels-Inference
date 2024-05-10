### Libraries ###
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

# Functions
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

# Simulation
### Set Up ###
set.seed(420)
ParameterVector = cbind(MisspecVec = c(rep(0,16),rep(1,16)),                         # Delta: Misspecification Rate
                        HomoskedVec = rep(c(rep(0,8), rep(0.5,8)),2),                # Gamma: Heteroskedasticity Rate
                        SizeVec = rep(c(rep(50,4), rep(200,4)),4),                   # N: Observations
                        LeverageVec = rep(c(0,0,0.1,0.1),8),                         # rho: Mixture Leverage
                        KVec = rep(c(1,5),16)                                        # K: Parameters
) %>% data.frame

### Run Simulation ###
NSim = 100
SimulationResultsList = list(length = nrow(ParameterVector))
for(i in 1:nrow(ParameterVector)){
  print(paste0("Case ",i))
  SimulationResultsList[[i]] = SimulationFunction(NSim = NSim, ParameterVector, i, TypeSetting = "Linear")
  #write.csv(SimulationResultsList[[i]]$SimulationSEResults, file ="data/SimulationCases/Linear/StandardErrors/Case",i,".csv")
  # write.csv(SimulationResultsList[[i]]$SimCoverageFrequency, file = "data/SimulationCases/Linear/Coverage/Case",i,".csv")
}
names(SimulationResultsList) = paste0("Case",1:nrow(ParameterVector))
saveRDS(SimulationResultsList, file = "data/SimulationCases/Linear/SimulationResults.rds")





