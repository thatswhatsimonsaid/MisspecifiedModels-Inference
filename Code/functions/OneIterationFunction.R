OneIterationFunction = function(N, rho, K, delta, gamma, ThetaPop, SimulationCase, type){
  
  ### IMPORTANT: Need to change to return confidence interval coverage ###
  
  ### Summary: Runs one iteration of a simulation according to Abadie, Imbens, Zheng (2014)
  ### Inputs:
    # N: Number of observations
    # rho: Mixture/leverage parameter
    # K: Number of covariates. Note the first covariate will always be a mixture 
        # of a standard normal and log normal(0,0.5^2)
    # delta: Misspecification parameter
    # gamma: Heterogeneity parameter
  ### Output:
  
    # RegressionSdErrEstimates: A 2x1 vector containing the population and conditional 
        # regression standard error estimates of X1
    # VarCovMatrixPop: The population variance-covariance matrix 
    # VarCovMatrixCond: The conditional variance-covariance matrix 

  # Set Up #
  SimulatedData = SimData(N = N, rho = rho, K = K, delta = delta, gamma = gamma, type = type)
  dat = SimulatedData$dat
  mu = SimulatedData$mu
  
  # Model #
  if(type == "Linear"){model = lm(Y~., data = dat)}else if(type == "Logistic"){
    model = glm(Y~., data = dat, family = "binomial")
    }
  beta_hat = as.numeric(model$coefficients)
  epsilon_hat = as.numeric(model$residuals)
  
  # Estimates #
  ThetaPop = ThetaPopFunction(dat = dat, SimulationCase = SimulationCase)
  ThetaCond = ThetaCondFunction(dat,mu)

  # Variance Estimates #
  # VPop = VHatPopFunctionLoop(dat, beta_hat)
  VCond = VHatCondFunctionLoop(dat, beta_hat, epsilon_hat)
  # VPop = VHatPopFunction(dat, beta_hat)
  # VCond = VHatCondFunction(dat, beta_hat, epsilon_hat)

  # VPopSE = VPop$RegressionSE
  VPopSE = sqrt(diag(vcovHC(model, type = "HC0")))
  VCondSE = VCond$RegressionSE 
  # VarCovMatrixPop = VPop$VarCovMatrix
  # VarCovMatrixCond = VCond$VarCovMatrix
  
  # VPopSE-sqrt(diag(vcovHC(model, type = "HC0"))) <1e-15 # Check if VPop == Standard Error from VCOV
  
  # Confidence Interval #
  CoverageResults = ConfidenceIntervalFunction(ThetaHat = beta_hat[2],
                                               ThetaCond = ThetaCond[2],
                                               ThetaPop = ThetaPop,
                                               SEPop = VPopSE[2],
                                               SECond = VCondSE[2],
                                               alpha = .05)
  
  # Output #
  RegressionSEEstimates = t(matrix(c(VPopSE[2],VCondSE[2])))
  colnames(RegressionSEEstimates) = c("Population", "Conditional")
  
  return(list(RegressionSdErrEstimates = RegressionSEEstimates,
              # VarCovMatrixPop = VarCovMatrixPop,
              # VarCovMatrixCond = VarCovMatrixCond,
              CoverageResults = CoverageResults))
}