VHatPopFunction = function(dat, beta_hat){
  
  ### Summary: Calculates the robust sandwich estimator of 
      # standard error according to Eicker, Huber, and White
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
  residuals = Y - X %*% beta_hat
  residuals_outer = diag(residuals %*% t(residuals)) 
  Meat = sapply(1:nrow(X), 
                function(i) residuals_outer[i] * (X[i,] %*% t(X[i,])))
  Meat = matrix(rowSums(Meat), nrow=ncol(X), ncol=ncol(X))/nrow(X)
  
  ### Sandwich Estimator ###
  VPop = Bread %*% Meat %*% Bread
  return(list(VarCovMatrix = VPop,
              RegressionSE = as.numeric(sqrt(diag(VPop)/nrow(X)))))
}