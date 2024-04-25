## Rm. Var.
rm(list=ls())
dir = "/Users/simondn/Documents/Stats572/"

## Libraries 
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(mixtools)
library(distr)

# Functions
source(paste0(dir,"Code/functions/AIZResults.R"))
source(paste0(dir,"Code/functions/ConfidenceIntervalFunction.R"))
source(paste0(dir,"Code/functions/OneIterationFunction.R"))
source(paste0(dir,"Code/functions/SimData.R"))
source(paste0(dir,"Code/functions/SimulationFunction.R"))
source(paste0(dir,"Code/functions/SimulationReformatResultsFunction.R"))
source(paste0(dir,"Code/functions/ThetaCondFunction.R"))
source(paste0(dir,"Code/functions/ThetaPopFunction.R"))
source(paste0(dir,"Code/functions/VHatCondFunction.R"))
source(paste0(dir,"Code/functions/VHatCondFunctionLoop.R"))
source(paste0(dir,"Code/functions/VHatPopFunction.R"))
source(paste0(dir,"Code/functions/VHatPopFunctionLoop.R"))
source(paste0(dir,"Code/functions/WhichMinFunction.R"))

### Set Up  ###
NSim = 1000
Binary = FALSE
ParameterVector = cbind(MisspecVec = c(rep(0,16),rep(0.1,16)),                       # Delta: Misspecification Rate
                        HomoskedVec = rep(c(rep(0,8), rep(0.5,8)),2),                # Gamma: Heteroskedasticity Rate
                        SizeVec = rep(c(rep(50,4), rep(200,4)),4),                   # N: Observations
                        LeverageVec = rep(c(0,0,0.1,0.1),8),                         # rho: Mixture Leverage
                        KVec = rep(c(1,5),16)                                        # K: Parameters
) %>% data.frame
CaseCheck = numeric(nrow(ParameterVector))


## Progress Bar ##
pb = txtProgressBar(min = 0, 
                    max = NSim,
                    style = 3,  
                    width = 50,
                    char = "=")

### Run ###
for(SimulationCase in 1:nrow(ParameterVector)){
  
  CheckEHW = numeric(NSim)
  print(SimulationCase) ,
    setTxtProgressBar(pb, i)
    
    delta = ParameterVector$MisspecVec[SimulationCase]  
    gamma =  ParameterVector$HomoskedVec[SimulationCase]  
    N = ParameterVector$SizeVec[SimulationCase]            
    rho =  ParameterVector$LeverageVec[SimulationCase]     
    K = ParameterVector$KVec[SimulationCase]   
    
    SimulatedData = SimData(N = N, rho = rho, K = K, delta = delta, gamma = gamma, Binary = Binary)
    dat = SimulatedData$dat
    mu = SimulatedData$mu
    
    # Model #
    if(Binary == FALSE){model = lm(Y~., data = dat)}else if(Binary == TRUE){
      model = glm(Y~., data = dat, family = "binomial")
    }
    beta_hat = as.numeric(model$coefficients)
    epsilon_hat = as.numeric(model$residuals)
    
    # Variance Estimates #
    VPopSE_MyLoop = VHatPopFunctionLoop(dat, beta_hat)$RegressionSE
    VPop_MyMatrix = VHatPopFunction(dat, beta_hat)$RegressionSE
    VPopSE_VCOV = sqrt(diag(vcovHC(model, type = "HC0")))
    
    CheckEHW[i] = var(c(VPopSE_MyLoop[2], VPop_MyMatrix[2], VPopSE_VCOV[2]))
  }
  CaseCheck[SimulationCase] = mean(CheckEHW)
}





