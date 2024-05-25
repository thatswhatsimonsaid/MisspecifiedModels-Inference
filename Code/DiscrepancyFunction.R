DiscrepancyFunction = function(epsilon, AIZResults, SimSEMedianList, CoverageList){
  
  ### Discrepancy ###
  Discrepancy = list()
  Discrepancy$SEPop = as.matrix(abs(AIZResults$MedianSEPop - SimSEMedianList$Population))
  Discrepancy$SECond = as.matrix(abs(AIZResults$MedianSECond - SimSEMedianList$Conditional))
  Discrepancy$CoveragePopPop = as.matrix(abs(AIZResults$ThetaPopVarPop - CoverageList$ThetaPop_VPop))
  Discrepancy$CoveragePopCond = as.matrix(abs(AIZResults$ThetaPopVarCond - CoverageList$ThetaPop_VCond))
  Discrepancy$CoverageCondPop = as.matrix(abs(AIZResults$ThetaCondVarPop - CoverageList$ThetaCond_VPop))
  Discrepancy$CoverageCondCond = as.matrix(abs(AIZResults$ThetaCondVarCond - CoverageList$ThetaCond_VCond))
  Discrepancy$Difference = as.matrix(abs(AIZResults$Difference - SimSEMedianList$Difference))
  
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