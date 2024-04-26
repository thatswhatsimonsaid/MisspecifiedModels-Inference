ThetaPopFunction = function(dat, delta, rho){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  

  ### EMu ###
  if(delta ==0.0 && rho ==0.0){EMu = 0}
  if(delta ==0.0 && rho ==0.1){EMu = 0.1*exp(1/8)}
  if(delta ==0.1 && rho ==0.0){EMu = 0}
  if(delta ==0.1 && rho ==0.1){EMu =0.1 *exp(1/8) + 0.001*exp(1/2) - 0.19}
  
  ### Set Up ###
  N = nrow(dat)
  K = ncol(dat)
  # Y = dat$Y
  X = select(dat,-Y) %>%
    mutate(X0 = rep(1,nrow(dat))) %>%
    relocate(X0, .before = X1) %>%
    as.matrix
  
  EMuVec =  rep(EMu,N)
  
  ### Estimate ###
  ThetaPop = (solve(t(X) %*% X) %*% (t(X) %*% EMuVec))[2]
  
  return(ThetaPop)
}
