OneIterationFunction = function(N, rho, K, delta, gamma, VarFixed, TypeSetting){
  
  ### Summary: Runs one iteration of a simulation according to Abadie, Imbens, 
  #            Zheng (2014)
  ### Inputs:
    # N: Number of observations
    # rho: Mixture/leverage parameter
    # K: Number of covariates. Note the first covariate will always be a mixture 
        # of a standard normal and log normal(0,0.5^2)
    # delta: Misspecification parameter
    # gamma: Heterogeneity parameter
    # VarFixed: Variables to condition on. VarFixed = NA means all covariates 
    #           were conditioned on.
  ### Output:
    # RegressionSdErrEstimates: A 2x1 vector with the population and conditional 
    #                           regression standard error estimates of X1
    # CoverageResults: Four binary values that indicate containment of the 
    #                  estimands in the four confidence intervals.

  # Set Up #
  if(TypeSetting == "Linear"){
    SimulatedData = SimDataLinear(N = N, 
                                  rho = rho, 
                                  K = K, 
                                  delta = 
                                    delta, 
                                  gamma = gamma)
    }else if(TypeSetting == "Logistic"){
      SimulatedData = SimDataLogistic(N = N, 
                                      rho = rho, 
                                      K = K, 
                                      delta = delta, 
                                      gamma = gamma)
    }
  dat = SimulatedData$dat
  mu = SimulatedData$mu
  
  # Model #
  if(TypeSetting == "Linear"){
    model = lm(Y~., data = dat)
    }else if(TypeSetting == "Logistic"){
    model = glm(Y~., data = dat, family = "binomial")
    }
  beta_hat = as.numeric(model$coefficients) 
  epsilon_hat = as.numeric(model$residuals)
  
  # Estimates #
  ThetaPop = ThetaPopFunction(rho_i = rho, 
                              N_i = N,
                              K_i = K, 
                              delta_i = delta, 
                              gamma_i = gamma, 
                              TypeSetting = TypeSetting)
  ThetaCond = ThetaCondFunction(dat, mu, TypeSetting = TypeSetting)

  # Variance Estimates #
  VCondSE = VHatCondFunction(dat, epsilon_hat, VarFixed)$RegressionSE
  # VPopSE = VHatPopFunction(dat, beta_hat)$RegressionSE                           # My pop. se function
  VPopSE = sqrt(diag(sandwich::vcovHC(model, type = "HC0")))                       # Pop. se function from sandwich package
  
  # Confidence Interval #
  CoverageResults = ConfidenceIntervalFunction(ThetaHat = beta_hat[2],
                                               ThetaCond = ThetaCond,
                                               ThetaPop = ThetaPop,
                                               SEPop = VPopSE[2],
                                               SECond = VCondSE[2],
                                               alpha = .05)
  
  # Output #
  RegressionSEEstimates = t(matrix(c(VPopSE[2],VCondSE[2])))
  colnames(RegressionSEEstimates) = c("Population", "Conditional")
  
  return(list(RegressionSdErrEstimates = RegressionSEEstimates,
              CoverageResults = CoverageResults))
}




