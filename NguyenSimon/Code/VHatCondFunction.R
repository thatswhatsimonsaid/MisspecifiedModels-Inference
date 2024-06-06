VHatCondFunction = function(dat, epsilon_hat, VarFixed = NA){
  
  ### Summary: Calculates the conditional variance estimator of 
      # Abadie, Imbens, and Zheng (2014).
  ### Input: 
    # dat: Data set
    # epsilon_hat: Estimated regression errors/residuals
    # VarFixed: Variables to condition on. Note the VarFixed = NA means no
    #           covariates were conditioned on.
  ### Output:
    # VarCovMatrix = The estimated conditional variance-covariance matrix
    # RegressionSE = The estimated conditional standard errors
  
  ### Check if Simulation or Application ###
  ApplicationIndicator = "cgb" %in% colnames(dat)
  
  ### Set Up ###
  N = nrow(dat)

  ### X Values ###
  if(ApplicationIndicator == TRUE){
    X = select(dat,-c(Y,name)) %>%
      mutate(X0 = rep(1,nrow(dat)),
             open65 = open*gdp65) %>%
      relocate(X0, .before = gdp65) %>%
      as.matrix
  }else if(ApplicationIndicator == FALSE){
    X = select(dat,-Y) %>%
      mutate(X0 = rep(1,nrow(dat))) %>%
      relocate(X0, .before = X1) %>%
      as.matrix
  }
  
  # Fixed Variables #
  if(any(is.na(VarFixed))){
    XSubset = select(data.frame(X), -c(X0))
    }else if(!any(is.na(VarFixed))){
    XSubset = data.frame(X)[VarFixed]} 
  
  ### lXi ### 
  dist_mah = StatMatch::mahalanobis.dist(XSubset)
  lXi = WhichMinFunction(dist_mah,2)
  eHat_lXi = epsilon_hat[lXi]
  X_lXi = X[lXi,]
  
  ### Bread ###
  XXt = t(X) %*% X
  Bread = solve(XXt/N)
  
  ### Meat ###
  Meat = t(epsilon_hat * X - eHat_lXi * X_lXi) %*% (epsilon_hat * X - eHat_lXi * X_lXi)/(2*nrow(X))
  
  ### Sandwich Estimator
  VCond = Bread %*% Meat %*% Bread
  return(list(VarCovMatrix = VCond,
              RegressionSE = as.numeric(sqrt(diag(VCond)/nrow(X)))))
}