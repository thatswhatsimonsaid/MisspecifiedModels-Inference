ThetaPopFunction = function(delta_i, rho_i, N_i, gamma_i, K_i, TypeSetting){
  
  ### Summary: Extracts the population regression estimand from the previously
  #            numerically calculated population estimand of the script 
  #            FindThetaPop.R.
  ### Input: 
  # N: Number of observations
  # rho: Mixture/leverage parameter
  # K: Number of covariates.
  # delta: Misspecification parameter
  # gamma: Heterogeneity parameter
  # TypeSetting: Linear/Logistic
  ### Output:
  # Population estimand
  
  if(TypeSetting == "Linear"){
    ThetaPopList = tryCatch({readRDS("/Users/simondn/Documents/Stats572/data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")}, 
                            error = function(e) {ThetaPopList <- readRDS("data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds")})
  }else if(TypeSetting == "Logistic"){
    ThetaPopList = tryCatch({readRDS("/Users/simondn/Documents/Stats572/data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")}, 
                            error = function(e) {ThetaPopList <-  readRDS("data/Logistic/EstimatedThetaPop/EstimatedThetaPop.rds")})
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

