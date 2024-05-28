ThetaPopFunction = function(delta_i, rho_i, N_i, gamma_i, K_i, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate

  if(TypeSetting == "Linear"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
    ThetaPopList = readRDS("data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")
  }else if(TypeSetting == "Logistic"){
    # ThetaPopList = readRDS("/Users/simondn/Documents/Stats572/data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
    ThetaPopList = readRDS("data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")
  }
  
  ### Assign Theta Pop ###
  ThetaPop = ThetaPopList %>% 
    filter(delta == delta_i) %>%
    filter(rho == rho_i) %>%
    filter(N == N_i) %>%
    filter(gamma == gamma_i) %>%
    filter(K == K_i) %>%
    select(ThetaPop) %>%
    as.numeric()
 
  return(ThetaPop)
}

