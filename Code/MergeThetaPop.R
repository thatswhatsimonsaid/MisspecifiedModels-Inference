# Libraries
library(tidyverse)
library(optparse)

# Rm. Var.
rm(list=ls())

# Parser
option_list = list(
  make_option(c("--TypeSetting"), type = "character", default = "Linear", help = "Linear vs. Logistic", metavar = "character"),
)
arg.parser = OptionParser(option_list = option_list)
args = parse_args(arg.parser)
TypeSetting = args$TypeSetting

# Files
FileList = list.files(path =  paste0("data/",TypeSetting,"/EstimatedThetaPop"), 
                      pattern = "\\.csv$", 
                      full.names = TRUE)
FileList = lapply(FileList, read.csv)

# ThetaPop
ThetaPopList = matrix(nrow = length(FileList), ncol = 6)
for(i in 1:length(FileList)){
  ThetaPopList[i,] = FileList[[i]]$x
}

# Reformat 
ThetaPopList = data.frame(ThetaPopList)
colnames(ThetaPopList) = c("delta","rho","N","gamma","K","ThetaPop")
ThetaPopList = ThetaPopList %>% mutate(ThetaPop = round(ThetaPop,2))

# Save
saveRDS(AllThetaPopList,paste0("data/",TypeSetting,"/EstimatedThetaPop/EstimatedThetaPop.rds"))





