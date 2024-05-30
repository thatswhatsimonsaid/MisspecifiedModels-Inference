SimulationFunction = function(NSim, delta, gamma, N, rho, K, VarFixed, TypeSetting){
  
  ### Summary: Runs the simulation of Abadie, Imbens, Zheng (2014).
  ### Inputs:
    # NSim: Number of simulations.
    # Delta: Misspecification rate.
    # Rho: Leverage rate.
    # N: Number of observations.
    # Gamma: Heteroscedasticity rate.
    # K: Number of covariates.    
    # VarFixed: Covariates to condition on. Note that VarFixed = NA means no 
    #           covariates were conditioned on.
    # TypeSetting: Linear/Logistic
  ### Output:
    # SimSEMedian: A (2x1) vector containing the median population and 
    #              conditional standard errors
    # SimulationSEResults: A (NSim x 2) matrix containing the standard error 
    #                      estimates of each iteration
    # SimCoverageFrequency: The four numerics indicating the percent in which 
    #                       the estimand is contained in the four confidence 
    #                       intervals.
    # Parameters: Parameters delta, gamma, N, rho, K of the simulation case
    # RunTime: A (NSim x 1) vector containing the time it took to run each 
    #          iteration
  
  ### Set Up ###
  SimulationSEResults = matrix(nrow = NSim, ncol = 2)
  SimulationCoverageResults = matrix(nrow = NSim, ncol = 4)
  
  ## Progress Bar ##
  pb = txtProgressBar(min = 0, 
                      max = NSim,
                      style = 3,  
                      width = 50,
                      char = "=")
  start_time = Sys.time()
  
  ### Algorithm ###
  for(i in 1:NSim){
    # Progress Bar #
    setTxtProgressBar(pb, i)
    
    # Simulation #
    OneIterationResults = OneIterationFunction(N = N, 
                                               rho = rho, 
                                               K = K, 
                                               delta = delta, 
                                               gamma = gamma,
                                               VarFixed = VarFixed,
                                               TypeSetting = TypeSetting)
    SimulationSEResults[i,] = OneIterationResults$RegressionSdErrEstimates
    SimulationCoverageResults[i,] = OneIterationResults$CoverageResults
    }
  
  ### System Time ###
  close(pb)
  end_time = Sys.time()
  run_time = end_time - start_time
  
  ### Reformat ###
  SimSEMedian = t(matrix(apply(SimulationSEResults, 2, median)))
  colnames(SimSEMedian) = c("Population", "Conditional")
  SimCoverageFrequency = matrix(colMeans(SimulationCoverageResults), ncol = 4)
  colnames(SimCoverageFrequency) = colnames(OneIterationResults$CoverageResults)

  ### Return ###
  return(list(SimSEMedian = SimSEMedian,
              SimulationSEResults = SimulationSEResults,
              SimCoverageFrequency = SimCoverageFrequency,
              Parameters =  list(delta = delta,
                                 gamma = gamma,
                                 N = N,
                                 rho = rho,
                                 K = K),
              RunTime = run_time))
}