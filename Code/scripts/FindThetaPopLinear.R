### Libraries ###
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(mixtools)
library(distr)

### Set Up ###
rm(list=ls())
TypeSetting = "Linear"
set.seed(420)

### Data Generating Process ###
SimDataLinear = function(N, rho, K, delta, gamma){
  ### Summary: Simulates Data according to the parameters of 
      # Abadie, Imbens, and Zhang (2014)
  ### Inputs:
    # N: Number of observations
    # rho: Mixture/leverage parameter
    # K: Number of covariates. Note the first covariate will always be 
        # a mixture of a standard normal and log normal(0,0.5^2)
    # delta: Misspecification parameter
    # gamma: Heterogeneity parameter
  ### Output:
    # dat: A data set
  
  ### Covariates ###
  X1 = sample(c(rnorm(N*(1-rho), mean = 0, sd = 1),
                rlnorm(N*rho, meanlog = 0, sdlog = sqrt(0.5))))
  X2K = sapply(1:(K-1), function(x) rnorm(n = N, mean = 0, sd = 1))
  
  ### Response ###
  mu_i = X1 + delta * (X1^2 - 1)
  sigma_i = exp(1-gamma*X1)
  Y = rnorm(n = N, mean = mu_i, sd = sigma_i)

  ### Return ###
  if(K == 1){dat = data.frame(Y, X1)}else if(K>1){dat = data.frame(Y, X1, X2K)}
  colnames(dat) = c("Y",paste0("X", 1:K))
  return(list(dat = dat,
              mu = mu_i))
}
               
NSim = 1000000
ParameterVector = cbind(MisspecVec = c(rep(0,16),rep(1,16)),                         # Delta: Misspecification Rate
                        HomoskedVec = rep(c(rep(0,8), rep(0.5,8)),2),                # Gamma: Heteroskedasticity Rate
                        SizeVec = rep(c(rep(50,4), rep(200,4)),4),                   # N: Observations
                        LeverageVec = rep(c(0,0,0.1,0.1),8),                         # rho: Mixture Leverage
                        KVec = rep(c(1,5),16)                                        # K: Parameters
) %>% data.frame
SimulationResultsList = list(length = nrow(ParameterVector))
beta_hat_simulation = numeric(nrow(ParameterVector))

pb = txtProgressBar(min = 0, 
                    max = NSim,
                    style = 3,  
                    width = 50,
                    char = "=")

### Run Simulation ###
for(SimulationCase in 1:nrow(ParameterVector)){
  
  print(paste0("Case",SimulationCase))
  
  ## Set Up ##
  beta_hat_list = numeric(length = NSim)
  
  ## Parameters ##
  delta = ParameterVector$MisspecVec[SimulationCase]  
  gamma =  ParameterVector$HomoskedVec[SimulationCase]  
  N = ParameterVector$SizeVec[SimulationCase]            
  rho =  ParameterVector$LeverageVec[SimulationCase]     
  K = ParameterVector$KVec[SimulationCase] 
  
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
  
  ## Save ##
  beta_hat_simulation[SimulationCase]  = mean(beta_hat_list)
}
beta_hat_simulation = data.frame(beta_hat_simulation)
# saveRDS(beta_hat_simulation, file = paste0(dir,"data/SimulationCases/",TypeSetting,"/beta_hat_",TypeSetting,"_New_simulation.rds"))
saveRDS(beta_hat_simulation, paste0("data/SimulationCases/Linear/beta_hat_Linear_simulation.rds"))

# mean(beta_hat_simulation[c(1,2,5,6,9,10,13,14),])            # Case 1: delta = 0, rho = 0.0
# mean(beta_hat_simulation[c(3,4,7,8,11,12,15,16),])           # Case 2: delta = 0, rho = 0.1
# mean(beta_hat_simulation[c(17,18,21,22,25,26,29,30),])       # Case 3: delta = 1, rho = 0.0
# beta_hat_simulation[c(19,20,23,24,27,28,31,32),]             # Case 4: delta = 1, rho = 0.1



