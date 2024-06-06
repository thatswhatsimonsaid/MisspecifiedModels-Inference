# Library
library(stats)
library(tidyverse)
library(StatMatch)
library(xtable)
library(lmtest)
library(sandwich)
library(ggplot2)
library(optparse)

# Set up
rm(list=ls())

# Parser
option_list = list(
  make_option(c("--TypeSetting"), type = "character", default = "Linear", help = "Linear vs. Logistic", metavar = "character")
)
arg.parser = OptionParser(option_list = option_list)
args = parse_args(arg.parser)
TypeSetting = args$TypeSetting

source("Code/AIZResults.R")
source("Code/DiscrepancyFunction.R")
source("Code/MonteCarloResults.R")
source("Code/SimulationReformatResultsFunction.R")

## Results
### Extract Results 
file_list = list.files(path =  paste0("Results/",TypeSetting,"/SimulationRDS"),
                       pattern = "\\.rds$",
                       full.names = TRUE)
SimulationResults_All = list()
SimulationResults_All = lapply(file_list, readRDS)
SimulationResults_All = SimulationReformatResultsFunction(SimulationResults_All)
rm(file_list)

### Large Results
LargeSampleSEMedianList = SimulationResults_All$SimSEMedianList
LargeSampleMonteCarloResults = MonteCarloResults(SimulationResults_All, 5)
LargeSampleRunTimeList = SimulationResults_All$RunTimeList  
LargeSampleCoverageList = data.frame(SimulationResults_All$CoverageList) 
LargeSampleParameterVector = SimulationResults_All$ParameterVector

LargeSampleResultsTable = cbind(LargeSampleSEMedianList[,c(1:5)],
                                LargeSampleCoverageList[,6:9],
                                LargeSampleSEMedianList[,6:10])
rownames(LargeSampleResultsTable) = paste0("Case ", 1:nrow(LargeSampleResultsTable)) 

LargeSampleResultsTable = LargeSampleResultsTable %>% mutate(Misspec. = case_when(Misspec. == 0 ~ "No",
                                                                                  Misspec. == 1 ~ "Yes"),
                                                             Homo. = case_when(Homo. == 0 ~ "Yes",
                                                                               Homo. == 0.5 ~ "No"),
                                                             Leverage = case_when(Leverage == 0 ~ "No",
                                                                                  Leverage == 0.1 ~ "Yes"))


SmallSampleSEMedianList = LargeSampleSEMedianList %>% filter(SS %in% c(50,200))
list(MonteCarliResults_Population = LargeSampleMonteCarloResults$MonteCarliResults_Population 
     %>% filter(SS %in% c(50,200)),
     MonteCarliResults_Conditional = LargeSampleMonteCarloResults$MonteCarliResults_Conditional 
     %>% filter(SS %in% c(50,200)),
     CombinedTables = LargeSampleMonteCarloResults$CombinedTables 
     %>% filter(SS %in% c(50,200))) -> SmallSampleMonteCarloResults

SmallSampleRunTimeList = LargeSampleRunTimeList %>% filter(SS %in% c(50,200))
SmallSampleCoverageList = LargeSampleCoverageList  %>% filter(SS %in% c(50,200))
SmallSampleParameterVector = LargeSampleParameterVector %>% filter(SS %in% c(50,200))

SmallSampleResultsTable = cbind(SmallSampleSEMedianList[,c(1:5)],
                                SmallSampleCoverageList[,6:9],
                                SmallSampleSEMedianList[,6:10])
rownames(SmallSampleResultsTable) = paste0("Case ", 1:nrow(SmallSampleResultsTable)) 

SmallSampleResultsTable = SmallSampleResultsTable %>% mutate(Misspec. = case_when(Misspec. == 0 ~ "No",
                                                                                  Misspec. == 1 ~ "Yes"),
                                                             Homo. = case_when(Homo. == 0 ~ "Yes",
                                                                               Homo. == 0.5 ~ "No"),
                                                             Leverage = case_when(Leverage == 0 ~ "No",
                                                                                  Leverage == 0.1 ~ "Yes"))

### Save Results
write.csv(SmallSampleResultsTable, paste0(dir,"Results/",TypeSetting,"/SmallSampleResultsTable.csv"))