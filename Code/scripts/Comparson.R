SEPrint = cbind(AIZ[,c(1:5,10,11)], SimSEMedianList[,c(6,7)])
colnames(SEPrint) = c("Misspec.",
                    "Homo.",
                    "SS",
                    "Leverage",
                    "K",
                    "MedianSE_Pop_AIZ",
                    "MedianSE_Cond_AIZ",
                    "MedianSE_Pop_Simon",
                    "MedianSE_Cond_Simon")
SEPrint = SEPrint[,c(1:5, 6,8, 7, 9)]
xtable(SEPrint)

CoveragePrint = cbind(AIZ[,c(1:5,8,9)], CoverageList[,c(3,4)])
colnames(CoveragePrint) = c("Misspec.",
                            "Homo.",
                            "SS",
                            "Leverage",
                            "K",
                            "Coverage_ThetaConditional_VarPopulation_AIZ",
                            "Coverage_ThetaConditional_VarConditional_AIZ",
                            "Coverage_ThetaConditional_VarPopulation_Simon",
                            "Coverage_ThetaConditional_VarConditional_Simon")
CoveragePrint = CoveragePrint[,c(1:5, 6,8,7,9)]
CoveragePrint %>% xtable()

