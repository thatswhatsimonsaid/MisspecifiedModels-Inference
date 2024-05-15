ThetaPopFunction = function(dat, rho, N, K, delta, gamma, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  

  # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/EstimatedThetaPop/EstimatedThetaPop.rds")
  ThetaPopList = readRDS("data/EstimatedThetaPop/EstimatedThetaPop.rds")
  InputVal = c(delta, gamma, N, rho, K)

  # Create a logical condition to filter rows
  Index = ThetaPopList$delta == InputVal[1] &
    ThetaPopList$gamma == InputVal[2] &
    ThetaPopList$N == InputVal[3] &
    ThetaPopList$rho == InputVal[4] &
    ThetaPopList$K == InputVal[5]
  ThetaPop = ThetaPopList[Index, 6]
  
  ### ThetaPop ###
  # for(i in 1:nrow(ParameterVector)){
  #   delta = ParameterVector$MisspecVec[i]
  #   gamma = ParameterVector$HomoskedVec[i]
  #   N = ParameterVector$SizeVec[i]
  #   rho = ParameterVector$LeverageVec[i]
  #   K = ParameterVector$KVec[i]
  #   
  #   
  #   InputVal = c(delta, rho, N, gamma, K)
  #   Index = ThetaPopList$delta == InputVal[1] &
  #     ThetaPopList$gamma == InputVal[2] &
  #     ThetaPopList$N == InputVal[3] &
  #     ThetaPopList$rho == InputVal[4] &
  #     ThetaPopList$K == InputVal[5]
  #   ThetaPop = ThetaPopList[Index, 6]
  #   
  #   print(data.frame(delta = delta,
  #                    gamma = gamma,
  #                    N = N,
  #                    rho = rho,
  #                    K = K,
  #                    ThetaPop = ThetaPop))
  # 
  # 
  #  # print(paste0("Case ", i, ": ",ThetaPop))
  # }
    
  return(ThetaPop)
}
