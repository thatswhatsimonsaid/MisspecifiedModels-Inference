### Summary: 
# Script used the parameter vector used for each case to numerically calculate
# the population estimand.
### Inputs:
# TypeSetting: Linear or logistic
# MisspecVec: A vector containing the values for misspecificaiton
# HomoskedVec: A vector containing the values for heteroscedasticity rate
# SizeVec: A vector containing the values for sample size
# LeverageVec: A vector containing the values for leverage rate
# KVec: A vector containing the values for number of covariates
# TypeSetting: Linear/Logistic
### Output:
# A csv file containing a combination of all parameters used to find the 
# conditional estimand

### Set Up ###
library(tidyverse)
rm(list=ls())
dir = "/Users/simondn/Documents/Stats572/data/Linear/Parameters/"
TypeSetting = "Linear"

ExpandGridCombinations= expand.grid(MisspecVec = c(0,1),
                                    HomoskedVec = c(0,0.5),
                                    SizeVec = c(seq(50, 2000, by = 150),9950),
                                    LeverageVec = c(0,0.1),
                                    KVec = c(1,5),
                                    TypeSetting = "Linear") %>% data.frame()


### Find Theta Pop ###
FTP_Combinations = ExpandGridCombinations %>% 
  mutate(JobName = paste0("FTP_",TypeSetting,"_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec),
         Output = paste0("data/",TypeSetting,"/EstimatedThetaPop/D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec, ".csv"))
write.csv(FTP_Combinations, file = paste0(dir,"ParameterVectorFindThetaPopALL.csv"))

### Run SLURM Simulations ###
Simulation_Combinations = ExpandGridCombinations %>%
  mutate(JobName = paste0("Sim_",TypeSetting,"_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec),
         Output = paste0("Results/",TypeSetting,"/LargeSampleExtension/sim_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec, ".rds"))
write.csv(Simulation_Combinations, file = paste0(dir,"ParameterVectorALL.csv"))