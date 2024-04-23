VHatCondFunctionLoop = function(dat, beta_hat, epsilon_hat){
  
  ### Summary: Calculates the conditional variance estimator of 
      # Abadie, Imbens, and Zheng (2014) with a for loop
  ### Input: 
    # dat: Data set
    # beta_hat: Estimated regression coefficients
    # epsilon_hat: Estimated regression errors/residuals
  ### Output:
    # VarCovMatrix = The conditional variance-covariance matrix
    # RegressionSE = The estimated conditional standard errors
  
  
  ### Set Up ###
  N = nrow(dat)
  Y = dat$Y
  X = select(dat,-Y) %>% 
    mutate(X0 = rep(1,nrow(dat))) %>%
    relocate(X0, .before = X1) %>%
    as.matrix
  
  ### lXi ###
  dist_mah = StatMatch::mahalanobis.dist(X[,2:ncol(X)])
  lXi = WhichMinFunction(dist_mah,2)
  eHat_lXi = epsilon_hat[lXi]
  X_lXi = X[lXi,]
  
  ### Bread ###
  XXt = t(X) %*% X
  Bread = solve(XXt/N)
  
  ### Meat ###
  Meat = array(numeric(), dim = c(ncol(X), ncol(X), nrow(X)))
  for(i in 1:nrow(X)){
    Meat[,,i] = (epsilon_hat[i]*X[i,] - eHat_lXi[i]*X_lXi[i,]) %*% 
      t(epsilon_hat[i]*X[i,] - eHat_lXi[i]*X_lXi[i,]) 
  }
  Meat = rowSums(Meat, dims = 2)/(2*nrow(X))
  
  ### Sandwich Estimator
  VCond = Bread * Meat * Bread
  return(list(VarCovMatrix = VCond,
              RegressionSE = as.numeric(sqrt(diag(VCond)/nrow(X)))))
}