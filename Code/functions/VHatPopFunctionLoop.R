VHatPopFunctionLoop = function(dat, beta_hat){
  
  ### Summary: Calculates the robust sandwich estimator of standard error according to Eicker, Huber, and White with a for loop
  ### Input: 
    # dat: Data set
    # beta_hat: Estimated regression coefficients
  ### Output:
  # VarCovMatrix = The population variance-covariance matrix
  # RegressionSE = The estimated population standard errors
  
  
  ### Set Up ###
  N = nrow(dat)
  Y = dat$Y
  X = select(dat,-Y) %>%
    mutate(X0 = rep(1,nrow(dat))) %>%
    relocate(X0, .before = X1) %>%
    as.matrix
  
  ### Bread ###
  XXt = t(X) %*% X
  Bread = solve(XXt/N)
  
  ### Meat ###
  Meat = array(numeric(), dim = c(ncol(X), ncol(X), nrow(X)))
  for(i in 1:nrow(X)){
    Meat[,,i] = as.numeric((Y[i] - X[i,] %*% beta_hat)^2) *  X[i,] %*% t(X[i,])
  }
  Meat = rowSums(Meat, dims = 2)/nrow(X)
  
  ### Sandwich Estimator ###
  VPop = Bread %*% Meat %*% Bread
  return(list(VarCovMatrix = VPop,
              RegressionSE = as.numeric(sqrt(diag(VPop)/nrow(X)))))
}