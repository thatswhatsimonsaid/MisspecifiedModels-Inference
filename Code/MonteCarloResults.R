MonteCarloResults = function(SimulationResultsReformatted, SigFigs){
  
  ### Set Up ###
  SimSEMedianList = SimulationResultsReformatted$SimSEMedianList 
  MonteCarloVariance = SimulationResultsReformatted$MonteCarloResults$MonteCarloVariance
  MonteCarloMean = SimulationResultsReformatted$MonteCarloResults$MonteCarloMean
  MonteCarloCI_Population = SimulationResultsReformatted$MonteCarloResults$MonteCarloCI_Population
  MonteCarloCI_Conditional = SimulationResultsReformatted$MonteCarloResults$MonteCarloCI_Conditional
  
  ### Population ###
  MonteCarliResults_Population = cbind(SimSEMedianList[,c(1:5)],
                                       MonteCarloMean[,1],
                                       MonteCarloVariance[,1],
                                       MonteCarloCI_Population)
  colnames(MonteCarliResults_Population) = c("Misspec.", 
                                             "Homo.",
                                             "SS",
                                             "Leverage",
                                             "K",
                                             "Monte Carlo Mean",
                                             "Monte Carlo Variance", 
                                             "MCLL", 
                                             "MCUL")
  
  MonteCarliResults_Population = MonteCarliResults_Population %>%
    mutate(paste0("(",as.character(round(MCLL,SigFigs)),",",as.character(round(MCUL,SigFigs)),")" ))
  
  colnames(MonteCarliResults_Population) = c("Misspec.", 
                                             "Homo.",
                                             "SS",
                                             "Leverage",
                                             "K",
                                             "Monte Carlo Mean",
                                             "Monte Carlo Variance", 
                                             "MCLL", 
                                             "MCUL", 
                                             "Monte Carlo Confidence Intervals")
  MonteCarliResults_Population = MonteCarliResults_Population %>% select(-c(MCLL,MCUL))
  MonteCarliResults_Population[,6:7] = round(MonteCarliResults_Population[,6:7],SigFigs)
  
  ### Conditional ###
  MonteCarliResults_Conditional = cbind(SimSEMedianList[,c(1:5)],
                                        MonteCarloMean[,2],
                                        MonteCarloVariance[,2],
                                        MonteCarloCI_Conditional)
  colnames(MonteCarliResults_Conditional) = c("Misspec.", 
                                              "Homo.",
                                              "SS",
                                              "Leverage",
                                              "K",
                                              "Monte Carlo Mean",
                                              "Monte Carlo Variance", 
                                              "MCLL", 
                                              "MCUL")
  
  MonteCarliResults_Conditional = MonteCarliResults_Conditional %>%
    mutate(paste0("(",as.character(round(MCLL,SigFigs)),",",as.character(round(MCUL,SigFigs)),")" ))
  
  colnames(MonteCarliResults_Conditional) = c("Misspec.", 
                                              "Homo.",
                                              "SS",
                                              "Leverage",
                                              "K",
                                              "Monte Carlo Mean",
                                              "Monte Carlo Variance", 
                                              "MCLL", 
                                              "MCUL", 
                                              "Monte Carlo Confidence Intervals")
  MonteCarliResults_Conditional = MonteCarliResults_Conditional %>% select(-c(MCLL,MCUL))
  MonteCarliResults_Conditional[,6:7] = round(MonteCarliResults_Conditional[,6:7],SigFigs)
  
  return(list(MonteCarliResults_Population = data.frame(MonteCarliResults_Population),
              MonteCarliResults_Conditional = data.frame(MonteCarliResults_Conditional)))
}