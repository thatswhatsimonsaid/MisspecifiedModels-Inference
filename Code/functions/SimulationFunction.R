SimulationFunction = function(NSim, ParameterVector, SimulationCase, type){
  
  ### Summary: Runs the simulation of Abadie, Imbens, Zheng (2014)
  ### Inputs:
    # NSim: Number of simulations
    # ParameterVector: A matrix containing the parameters delta, gamma, N, rho, and K.
    # SimulationCase: Which row of the parameter vector to run. 
    #                 Simulation cases are the same case as the table in Abadie, Imbens, Zheng (2014)
  ### Output:
    # SimulationSEResults: A (NSim x 2) matrix containing the standard error estimates of each iteration
    # SimSEMedian: A (2x1) vector containing the median population and conditional standard errors
    # Parameters: Parameters delta, gamma, N, rho, K of the simulation case
    # RunTime: A (NSim x 1) vector containing the time it took to run each iteration
  
  ### Set Up ###
  SimulationSEResults = matrix(nrow = NSim, ncol = 2)
  SimulationCoverageResults = matrix(nrow = NSim, ncol = 4)
  
  ## Parameters ##
  delta = ParameterVector$MisspecVec[SimulationCase]  
  gamma =  ParameterVector$HomoskedVec[SimulationCase]  
  N = ParameterVector$SizeVec[SimulationCase]            
  rho =  ParameterVector$LeverageVec[SimulationCase]     
  K = ParameterVector$KVec[SimulationCase]    
  
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
                                               ThetaPop = ThetaPop,
                                               type = type)
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
  return(list(SimulationSEResults = SimulationSEResults,
              SimSEMedian = SimSEMedian,
              SimCoverageFrequency = SimCoverageFrequency,
              Parameters =  list(delta = delta,
                                 gamma = gamma,
                                 N = N,
                                 rho = rho,
                                 K = K),
              RunTime = run_time))
}