ThetaPopFunction = function(rho, N, K, delta, gamma, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  
  ### Simulation Cases ###
  SimulationCase = SimulationCaseFunction(delta, gamma, N, rho, K)

  if(TypeSetting == "Linear"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
    # ThetaPopList = readRDS("data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")

    ### Small Sample ###
    if(SimulationCase <= 18){ThetaPop = 1.0}
    if(SimulationCase %in% c(21,22,25,26,29,30)){ThetaPop = 1.0}
    if(SimulationCase %in% c(19,20,27,28)){ThetaPop = 1.5}
    if(SimulationCase %in% c(23,24,31,32)){ThetaPop = 1.6}
    
    ### Large Sample ###
    if(!(SimulationCase %in% c(21,22,25,26,29,30,
                              19,20,27,28,
                              23,24,31,32, 
                              1023,1024,1031,1032,
                              1019,1020,1027,1028)) & SimulationCase>18){ThetaPop = 1.0}
    if(SimulationCase %in% c(1019,1020,1027,1028)){ThetaPop = 1.65}
    if(SimulationCase %in% c(1023,1024,1031,1032)){ThetaPop = 1.68}
    
    
    ### Large Sample ###
    if(SimulationCase <= 18){ThetaPop = 1.0}
    if(SimulationCase %in% c(21,22,25,26,29,30)){ThetaPop = 1.0}
    if(SimulationCase %in% c(19,20,27,28)){ThetaPop = 1.5}
    if(SimulationCase %in% c(23,24,31,32)){ThetaPop = 1.6}
    
  }else if(TypeSetting == "Logistic"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
    # ThetaPopList = readRDS("data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
  }
  
  # InputVal = c(delta, rho, N, gamma, K)

  # Create a logical condition to filter rows
  # Index = ThetaPopList$delta == InputVal[1] &
  #   ThetaPopList$gamma == InputVal[2] &
  #   ThetaPopList$N == InputVal[3] &
  #   ThetaPopList$rho == InputVal[4] &
  #   ThetaPopList$K == InputVal[5]
  # ThetaPop = ThetaPopList[Index, 6]
    
  return(ThetaPop)
}
