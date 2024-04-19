VHatCondFunction = function(dat, beta_hat, epsilon_hat){
  
  
  # IMPORTANT: CURRENTLY NOT SURE IF IT WORKS HAHA #
  
  ### Summary: Calculates the conditional variance estimator of Abadie, Imbens, and Zheng (2014)
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
    # mutate(X0 = rep(1,nrow(dat))) %>%
    # relocate(X0, .before = X1) %>%
    as.matrix

  XXt = diag( X %*% t(X))

  ### lXi ###
  # dist_mah = StatMatch::mahalanobis.dist(X[,2:ncol(X)])
  dist_mah = StatMatch::mahalanobis.dist(X[,1:ncol(X)])
  lXi = WhichMinFunction(dist_mah,2)
  eHat_lXi = epsilon_hat[lXi]
  X_lXi = X[lXi,]

  ### Bread ###
  Bread = solve(sum(XXt)/N)

  ### Meat ###

  ## Edited
  eHatX = epsilon_hat * X
  eLHatXl = eHat_lXi * X_lXi
  Meat = sum(diag((eHatX - eLHatXl) %*% t(eHatX - eLHatXl)))/(2*N)

  ## Old ##
  # eHatX = epsilon_hat %*% X
  # eLHatXl = eHat_lXi %*% X_lXi
  # Meat = ((eHatX - eLHatXl) %*% t(eHatX - eLHatXl))/(2*N)

  ### Sandwich Estimator
  VCond = as.numeric(Bread * Meat * Bread)
  return(VCond)
}