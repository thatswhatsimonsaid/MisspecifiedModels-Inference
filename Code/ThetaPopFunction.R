ThetaPopFunction = function(delta_i, rho_i, N_i, gamma_i, K_i, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate

  if(TypeSetting == "Linear"){
    ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
    # ThetaPopList = readRDS("data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
    ThetaPop = ThetaPopList %>% 
      filter(delta == delta_i) %>%
      filter(rho == rho_i) %>%
      filter(N == N_i) %>%
      filter(gamma == gamma_i) %>%
      filter(K == K_i) %>%
      select(ThetaPop) %>%
      as.numeric()
  }
    
  else if(TypeSetting == "Logistic"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
    # ThetaPopList = readRDS("data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
  }
 
  return(ThetaPop)
}


# ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
# for(i in 1:nrow(ParameterVector)){
#   print(paste0("Case: ",i))
#   
#   delta = ParameterVector$MisspecVec[i]
#   rho = ParameterVector$LeverageVec[i]
#   N = ParameterVector$SizeVec[i]
#   gamma = ParameterVector$HomoskedVec[i]
#   K = ParameterVector$KVec[i]
#   ThetaPopOutput = ThetaPopFunction(rho = rho, N = N, K = K, delta = delta, gamma = gamma, "Linear")
#   ThetaPopExpected = ThetaPopList[i,]$ThetaPop
#   print(c(i,ThetaPopOutput,ThetaPopExpected))
#   print('---')
# }

