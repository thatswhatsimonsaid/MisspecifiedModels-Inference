DiscrepancyFunction = function(epsilon, AIZResults, SimSEMedianList, CoverageList){

  ### Discrepancy ###
  Discrepancy = list()
  Discrepancy$SEPop = as.matrix(abs(AIZ$MedianSEPop - SimSEMedianList$Population))
  Discrepancy$SECond = as.matrix(abs(AIZ$MedianSECond - SimSEMedianList$Conditional))
  Discrepancy$CoveragePopPop = as.matrix(abs(AIZ$ThetaPopVarPop - CoverageList[,1]))
  Discrepancy$CoveragePopCond = as.matrix(abs(AIZ$ThetaPopVarCond - CoverageList[,2]))
  Discrepancy$CoverageCondPop = as.matrix(abs(AIZ$ThetaCondVarPop - CoverageList[,3]))
  Discrepancy$CoverageCondCond = as.matrix(abs(AIZ$ThetaCondVarCond - CoverageList[,4]))
  Discrepancy$Difference = as.matrix(abs(AIZ$Difference - SimSEMedianList$Difference))
  
  ### Indices ###
  LargeDiscrepancyIndices = list()
  LargeDiscrepancyIndices$SEPop = which(Discrepancy$SEPop >=epsilon)
  LargeDiscrepancyIndices$SECond = which(Discrepancy$SECond >=epsilon)
  LargeDiscrepancyIndices$CoveragePopPop = which(Discrepancy$CoveragePopPop >=epsilon)
  LargeDiscrepancyIndices$CoveragePopCond = which(Discrepancy$CoveragePopCond >=epsilon)
  LargeDiscrepancyIndices$CoverageCondPop = which(Discrepancy$CoverageCondPop >=epsilon)
  LargeDiscrepancyIndices$CoverageCondCond = which(Discrepancy$CoverageCondCond >=epsilon)
  LargeDiscrepancyIndices$Difference = which(Discrepancy$Difference >=epsilon)
  
  ### Average ###
  MeanDiscrepancy = list()
  MeanDiscrepancy$Difference = mean(Discrepancy$Difference)
  MeanDiscrepancy$SEPop = mean(Discrepancy$SEPop)
  MeanDiscrepancy$SECond = mean(Discrepancy$SECond)
  MeanDiscrepancy$CoveragePopPop = mean(Discrepancy$CoveragePopPop)
  MeanDiscrepancy$CoveragePopCond = mean(Discrepancy$CoveragePopCond)
  MeanDiscrepancy$CoverageCondPop = mean(Discrepancy$CoverageCondPop)
  MeanDiscrepancy$CoverageCondCond = mean(Discrepancy$CoverageCondCond)
  
  return(list(Discrepancy = Discrepancy,
              LargeDiscrepancyIndices = LargeDiscrepancyIndices,
              MeanDiscrepancy = MeanDiscrepancy))
  }