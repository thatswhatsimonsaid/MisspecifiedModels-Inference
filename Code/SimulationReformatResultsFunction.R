SimulationReformatResultsFunction = function(SimulationResultsList){
  
  ### Summary: Just reformats the data into the simulation table in Abadie, Imbens, Zheng (2014)
  ### Inputs:
  # SimulationResultsList: Simulation results from the SimulationFunction.R
  ### Outputs:
  # ParameterVector: Vector of Parameters for each case
  # MonteCarloVariance: Monte carlo simulation variance across simulations
  # CoverageList: A list of coverages using a combination of {theta_pop,theta_cond} and {Vpop,Vcond}
  # SimSEMedianList: Simulation results table reformatted into the format of Abadie, Imbens, Zheng (2014)
  # RunTimeList: Run time of each case
  
  ### Set Up ###
  NSim = length(SimulationResultsList)
  alpha = 0.05
  SimSEMedianList = numeric(NSim*9) %>% matrix(ncol = 9)
  MonteCarloMean = numeric(NSim*2) %>% matrix(ncol = 2)
  MonteCarloVariance = numeric(NSim*2) %>% matrix(ncol = 2)
  MonteCarloCI_Population = numeric(NSim*2) %>% matrix(ncol = 2)
  MonteCarloCI_Conditional = numeric(NSim*2) %>% matrix(ncol = 2)
  CoverageList = numeric(NSim*4) %>% matrix(ncol = 4)
  RunTimeList = as.matrix(numeric(NSim))
  
  ### Fill in values ###
  for(i in 1:NSim){
    
    ### Run Times ###
    RunTimeList[i] = SimulationResultsList[[i]]$RunTime
    
    ## Monte Carlo ##
    MonteCarloMean[i,] = SimulationResultsList[[i]]$SimulationSEResults %>% colMeans
    MonteCarloVariance[i,] = apply(X = SimulationResultsList[[i]]$SimulationSEResults, MARGIN = 2, FUN = var)
    MonteCarloCI_Population[i,] = MonteCarloMean[i,1] + outer(MonteCarloVariance[i,1]/NSim*qnorm(1-alpha/2,0,1), c(-1,1))
    MonteCarloCI_Conditional[i,] = MonteCarloMean[i,2] + outer(MonteCarloVariance[i,1]/NSim*qnorm(1-alpha/2,0,1), c(-1,1))
    
    colnames(MonteCarloMean) = c("Population", "Conditional")
    colnames(MonteCarloVariance) = c("Population", "Conditional")
    colnames(MonteCarloCI_Population) = c("LL", "UL")
    colnames(MonteCarloCI_Conditional) = c("LL", "UL")
    
    ### Standard Errors ###
    SimSEMedianList[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    SimSEMedianList[i,6:7] = as.numeric(SimulationResultsList[[i]]$SimSEMedian)

    ### Coverage ###
    CoverageList[i,] = as.numeric(SimulationResultsList[[i]]$SimCoverageFrequency)
  }
  
  ### Population > Conditional ###
  # Difference #
  SimSEMedianList[,8] = SimSEMedianList[,6] - SimSEMedianList[,7]
  
  # Indicator #
  SimSEMedianList[,9] = SimSEMedianList[,8] > 0
  
  ### Reformatting ###
  # Run Time #
  rownames(RunTimeList) = paste0(1:NSim)

  # Coverage List #
  colnames(CoverageList) = c("ThetaPop_VPop", "ThetaPop_VCond", "ThetaCond_VPop", "ThetaCond_VCond")
  rownames(CoverageList) = paste0(1:NSim)
  
  # Standard Error List #
  colnames(SimSEMedianList) = c("Misspec.", 
                                "Homo.", 
                                "SS", 
                                "Leverage", 
                                "K", 
                                "Population", 
                                "Conditional", 
                                "Difference", 
                                "Indicator")
  rownames(SimSEMedianList) = paste0(1:NSim)
  SimSEMedianList = as.data.frame(SimSEMedianList)
  # SimSEMedianList = SimSEMedianList %>% mutate(Misspec. = case_when(Misspec. == 0 ~ "No",
  #                                                                   Misspec. == 1 ~ "Yes"),
  #                                              Homo. = case_when(Homo. == 0 ~ "Yes",
  #                                                                Homo. == 0.5 ~ "No"),
  #                                              Leverage = case_when(Leverage == 0 ~ "No",
  #                                                                   Leverage == 0.1 ~ "Yes"))
  ParameterVector = SimSEMedianList[,1:5]
  return(list(SimSEMedianList = SimSEMedianList,
              MonteCarloResults = list(MonteCarloMean = MonteCarloMean, 
                                       MonteCarloVariance = MonteCarloVariance, 
                                       MonteCarloCI_Population = MonteCarloCI_Population,
                                       MonteCarloCI_Conditional = MonteCarloCI_Conditional),
              ParameterVector = ParameterVector,
              CoverageList = CoverageList,
              RunTimeList = RunTimeList))
}