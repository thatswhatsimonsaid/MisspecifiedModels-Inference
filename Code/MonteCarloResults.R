### Summary:
# Extracts the monte carlo results across all NSim simulations for each of the 
# simulation cases.
### Inputs:
# SimulationResultsReformatted: Reformatted simulation results.
# SigFigs: Number of significant figures
### Output:
# Each of the following table will contain a column for each of the parameters 
# used in the simulation, the average standard error estimate across NSim 
# simulations, the variance of the standard error estimate across NSim 
# simulations, and the monte carlo confidence interval. 
# MonteCarliResults_Population contains this monte carlo results for only the 
# population estimand, MonteCarliResults_Conditional contains this monte carlo 
# results for only the CombinedTables, contains the monte carlo results for
# both the population and conditional estimand.


MonteCarloResults = function(SimulationResultsReformatted, SigFigs){
  
  ### Set Up ###
  SimSEMedianList = SimulationResultsReformatted$SimSEMedianList 
  MonteCarloVariance = SimulationResultsReformatted$MonteCarloResults$MonteCarloVariance
  MonteCarloMean = SimulationResultsReformatted$MonteCarloResults$MonteCarloMean
  MonteCarloCI_Population = SimulationResultsReformatted$MonteCarloResults$MonteCarloCI_Population
  MonteCarloCI_Conditional = SimulationResultsReformatted$MonteCarloResults$MonteCarloCI_Conditional
  
  ### Population ###
  MonteCarliResults_Population = cbind(MonteCarloMean[,1:5],
                                       MonteCarloMean$Population,
                                       MonteCarloVariance$Population,
                                       MonteCarloCI_Population$LL,
                                       MonteCarloCI_Population$UL)
  
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
    mutate(paste0("(",as.character(round(MCLL,SigFigs)),",",
                  as.character(round(MCUL,SigFigs)),")" ))
  
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
  MonteCarliResults_Population = MonteCarliResults_Population %>% 
    select(-c(MCLL,MCUL))
  MonteCarliResults_Population[,6:7] = MonteCarliResults_Population[,6:7] %>%
    round(SigFigs)
  
  ### Conditional ###
  MonteCarliResults_Conditional = cbind(MonteCarloMean[,1:5],
                                        MonteCarloMean$Conditional,
                                        MonteCarloVariance$Conditional,
                                        MonteCarloCI_Conditional$LL,
                                        MonteCarloCI_Conditional$UL)
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
    mutate(paste0("(",as.character(round(MCLL,SigFigs)),",",
                  as.character(round(MCUL,SigFigs)),")" ))
  
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
  MonteCarliResults_Conditional = MonteCarliResults_Conditional %>% 
    select(-c(MCLL,MCUL))
  MonteCarliResults_Conditional[,6:7] = MonteCarliResults_Conditional[,6:7] %>%
    round(SigFigs)
  
  ### Arrange ###
  MonteCarliResults_Population = data.frame(MonteCarliResults_Population) %>% 
    arrange(Misspec., Homo., SS, Leverage, K)
  MonteCarliResults_Conditional = data.frame(MonteCarliResults_Conditional) %>% 
    arrange(Misspec., Homo., SS, Leverage, K)
  
  
  ### Combined Table ###
  CombinedTables = cbind(MonteCarliResults_Population, 
                         MonteCarliResults_Conditional[,6:8])
  colnames(CombinedTables) = c("Misspec.", 
                               "Homo.",
                               "SS",
                               "Leverage",
                               "K",
                               "Monte Carlo Mean - Pop",
                               "Monte Carlo Variance - Pop",
                               "Monte Carlo Confidence Intervals - Pop",
                               "Monte Carlo Mean - Cond",
                               "Monte Carlo Variance - Cond",
                               "Monte Carlo Confidence Intervals - Cond")
  
  return(list(MonteCarliResults_Population = MonteCarliResults_Population,
              MonteCarliResults_Conditional = MonteCarliResults_Conditional,
              CombinedTables = CombinedTables))
}