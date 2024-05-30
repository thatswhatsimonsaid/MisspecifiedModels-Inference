### Set Up ###
library(tidyverse)
rm(list=ls())
TypeSetting = "Linear"
ExpandGridCombinations= expand.grid(MisspecVec = c(0,1),
                                    HomoskedVec = c(0,0.5),
                                    SizeVec = seq(50, 10000, by = 150),
                                    LeverageVec = c(0,0.1),
                                    KVec = c(1,5),
                                    TypeSetting = "Linear") %>% data.frame()


### Find Theta Pop ###
FTP_Combinations = ExpandGridCombinations %>% 
  mutate(JobName = paste0("FTP_",TypeSetting,"_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec),
         Output = paste0("data/",TypeSetting,"/EstimatedThetaPop/D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec, ".csv"))
write.csv(FTP_Combinations, file = "/Users/simondn/Documents/Stats572/data/Linear/Parameters/ParameterVectorFindThetaPopALL.csv")

### Run SLURM Simulations ###
Simulation_Combinations = ExpandGridCombinations %>%
  mutate(JobName = paste0("Sim_",TypeSetting,"_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec),
         Output = paste0("Results/",TypeSetting,"/LargeSampleExtension/sim_D",MisspecVec,"_G",HomoskedVec,"_N",SizeVec,"_R",LeverageVec,"_K",KVec, ".rds"))
write.csv(Simulation_Combinations, file = "/Users/simondn/Documents/Stats572/data/Linear/Parameters/ParameterVectorALL.csv")