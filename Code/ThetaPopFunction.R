ThetaPopFunction = function(dat, rho, N, K, delta, gamma, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  
  if(TypeSetting == "Linear"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
    ThetaPopList = readRDS("data/EstimatedThetaPop/EstimatedThetaPop.rds")
  }else if(TypeSetting == "Logistic"){
    ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
    # ThetaPopList = readRDS("data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
  }
  
  InputVal = c(delta, rho, N, gamma, K)

  # Create a logical condition to filter rows
  Index = ThetaPopList$delta == InputVal[1] &
    ThetaPopList$gamma == InputVal[2] &
    ThetaPopList$N == InputVal[3] &
    ThetaPopList$rho == InputVal[4] &
    ThetaPopList$K == InputVal[5]
  ThetaPop = ThetaPopList[Index, 6]
    
  return(ThetaPop)
}
