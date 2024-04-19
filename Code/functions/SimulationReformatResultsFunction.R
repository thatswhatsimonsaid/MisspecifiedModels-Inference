SimulationReformatResultsFunction = function(SimulationResultsList){
  
  ### Summary: Just reformats the data into the simulation table in Abadie, Imbens, Zheng (2014)
  ### Inputs:
  # SimulationResultsList: Simulation results from the SimulationFunction.R
  ### Outputs:
  # ParameterVector: Vector of Parameters for each case
  # CoverageList: A list of coverages using a combination of {theta_pop,theta_cond} and {Vpop,Vcond}
  # SimSEMedianList: Simulation results table reformatted into the format of Abadie, Imbens, Zheng (2014)
  # RunTimeList: Run time of each case
  
  ### Set Up ###
  SimSEMedianList = numeric(length(SimulationResultsList)*9) %>% matrix(ncol = 9)
  CoverageList = numeric(length(SimulationResultsList)*4) %>% matrix(ncol = 4)
  RunTimeList = as.matrix(numeric(length(SimulationResultsList)))
  
  ### Fill in values ###
  for(i in 1:length(SimulationResultsList)){
    
    ### Run Times ###
    RunTimeList[i] = SimulationResultsList[[i]]$RunTime
    
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
  rownames(RunTimeList) = paste0(1:32)

  # Coverage List #
  colnames(CoverageList) = c("ThetaPop_VPop", "ThetaPop_VCond", "ThetaCond_VPop", "ThetaCond_VCond")
  rownames(CoverageList) = paste0(1:32)
  
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
  rownames(SimSEMedianList) = paste0(1:length(SimulationResultsList))
  SimSEMedianList = as.data.frame(SimSEMedianList)
  SimSEMedianList = SimSEMedianList %>% mutate(Misspec. = case_when(Misspec. == 0 ~ "No",
                                                                    Misspec. == 0.1 ~ "Yes"),
                                               Homo. = case_when(Homo. == 0 ~ "Yes",
                                                                 Homo. == 0.5 ~ "No"),
                                               Leverage = case_when(Leverage == 0 ~ "No",
                                                                    Leverage == 0.1 ~ "Yes"))
  ParameterVector = SimSEMedianList[,1:5]
  return(list(SimSEMedianList = SimSEMedianList,
              ParameterVector = ParameterVector,
              CoverageList = CoverageList,
              RunTimeList = RunTimeList))
}