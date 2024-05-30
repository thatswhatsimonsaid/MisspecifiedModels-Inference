SimulationReformatResultsFunction = function(SimulationResultsList){
  
  ### Summary: Just reformats the data into the simulation table in Abadie, 
  #            Imbens, Zheng (2014)
  ### Inputs:
  # SimulationResultsList: Simulation results from the SimulationFunction.R
  ### Outputs:
  # SimSEMedianList: Simulation results table reformatted into the format of 
  #                  the original manuscript.
  # MonteCarloVariance: Monte carlo simulation variance across simulations
  # ParameterVector: Vector of Parameters for each case
  # CoverageList: A list of coverages using a combination of the 
  #               population/conditional estimand and population/conditional
  #               standard error estimate.
  # RunTimeList: Run time of each case
  
  ### Set Up ###
  NCases = length(SimulationResultsList)
  alpha = 0.05
  SimSEMedianList = numeric(NCases*7) %>% matrix(ncol = 7)
  MonteCarloMean = numeric(NCases*7) %>% matrix(ncol = 7)
  MonteCarloVariance = numeric(NCases*7) %>% matrix(ncol = 7)
  MonteCarloCI_Population = numeric(NCases*7) %>% matrix(ncol = 7)
  MonteCarloCI_Conditional = numeric(NCases*7) %>% matrix(ncol = 7)
  CoverageList = numeric(NCases*9) %>% matrix(ncol = 9)
  RunTimeList = numeric(NCases*6) %>% matrix(ncol = 6)
  
  ### Fill in values ###
  for(i in 1:NCases){
    
    ### Parameters ###
    RunTimeList[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    CoverageList[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    SimSEMedianList[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    MonteCarloMean[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    MonteCarloVariance[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    MonteCarloCI_Population[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    MonteCarloCI_Conditional[i,1:5] = as.numeric(SimulationResultsList[[i]]$Parameters)
    
    ### Run Times ###
    RunTimeList[i,6] = SimulationResultsList[[i]]$RunTime
    
    ## Monte Carlo ##
    MonteCarloMean[i,6:7] = SimulationResultsList[[i]]$SimulationSEResults %>% colMeans
    MonteCarloVariance[i,6:7] = apply(X = SimulationResultsList[[i]]$SimulationSEResults, MARGIN = 2, FUN = var)
    MonteCarloCI_Population[i,6:7] = MonteCarloMean[i,6] + outer(sqrt(MonteCarloVariance[i,6]/NCases)*qnorm(1-alpha/2,0,1), c(-1,1))
    MonteCarloCI_Conditional[i,6:7] = MonteCarloMean[i,7] + outer(sqrt(MonteCarloVariance[i,7]/NCases)*qnorm(1-alpha/2,0,1), c(-1,1))

    ### Standard Errors ###
    SimSEMedianList[i,6:7] = as.numeric(SimulationResultsList[[i]]$SimSEMedian)

    ### Coverage ###
    CoverageList[i,6:9] = as.numeric(SimulationResultsList[[i]]$SimCoverageFrequency)
    
  }
  
  ### Column Names ###
  colnames(RunTimeList) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "RunTime")
  colnames(CoverageList) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "ThetaPop_VPop", "ThetaPop_VCond", "ThetaCond_VPop", "ThetaCond_VCond")
  colnames(SimSEMedianList) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "Population", "Conditional")
  colnames(MonteCarloMean) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "Population", "Conditional")
  colnames(MonteCarloVariance) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "Population", "Conditional")
  colnames(MonteCarloCI_Population) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "LL", "UL")
  colnames(MonteCarloCI_Conditional) = c("Misspec.", "Homo.", "SS", "Leverage", "K", "LL", "UL")
  
  ### Reformat ###
  RunTimeList = data.frame(RunTimeList) %>% arrange(Misspec., Homo., SS, Leverage, K)
  CoverageList = data.frame(CoverageList) %>% arrange(Misspec., Homo., SS, Leverage, K)
  SimSEMedianList = data.frame(SimSEMedianList) %>% arrange(Misspec., Homo., SS, Leverage, K)
  MonteCarloMean = data.frame(MonteCarloMean) %>% arrange(Misspec., Homo., SS, Leverage, K)
  MonteCarloVariance = data.frame(MonteCarloVariance) %>% arrange(Misspec., Homo., SS, Leverage, K)
  MonteCarloCI_Population = data.frame(MonteCarloCI_Population) %>% arrange(Misspec., Homo., SS, Leverage, K)
  MonteCarloCI_Conditional = data.frame(MonteCarloCI_Conditional) %>% arrange(Misspec., Homo., SS, Leverage, K)
  ParameterVector = SimSEMedianList[,1:5]
  
  
  ### Population > Conditional ###
  SimSEMedianList = SimSEMedianList %>% mutate(Difference = Population-Conditional,
                                               Indicator = Difference >0,
                                               PercentChange = (Conditional-Population)/Population )

  
  ### Row Names ###
  rownames(RunTimeList) = paste0(1:NCases)
  rownames(CoverageList) =paste0(1:NCases)
  rownames(SimSEMedianList) = paste0(1:NCases)
  rownames(MonteCarloMean) =paste0(1:NCases)
  rownames(MonteCarloVariance) =paste0(1:NCases)
  rownames(MonteCarloCI_Population) = paste0(1:NCases)
  rownames(MonteCarloCI_Conditional) = paste0(1:NCases)

  
  # SimSEMedianList = SimSEMedianList %>% mutate(Misspec. = case_when(Misspec. == 0 ~ "No",
  #                                                                   Misspec. == 1 ~ "Yes"),
  #                                              Homo. = case_when(Homo. == 0 ~ "Yes",
  #                                                                Homo. == 0.5 ~ "No"),
  #                                              Leverage = case_when(Leverage == 0 ~ "No",
  #                                                                   Leverage == 0.1 ~ "Yes"))

  return(list(SimSEMedianList = SimSEMedianList,
              MonteCarloResults = list(MonteCarloMean = MonteCarloMean,
                                       MonteCarloVariance = MonteCarloVariance,
                                       MonteCarloCI_Population = MonteCarloCI_Population,
                                       MonteCarloCI_Conditional = MonteCarloCI_Conditional),
              ParameterVector = ParameterVector,
              CoverageList = CoverageList,
              RunTimeList = RunTimeList))
}