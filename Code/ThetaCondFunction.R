ThetaCondFunction = function(dat, mu, TypeSetting){
  
  ### Summary: Calculates the conditional regression estimate 
      #  according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # mu: the N-vector whose i’th element is equal to µ_{Xi}, 
      # which is the mean of Y_i|X_{1i},...,X_{Ki}
  ### Output:
  # Conditional regression estimate
  
  ### Set Up ###
  N = nrow(dat)
  Y = dat$Y
  # X = select(dat,-Y) %>%
  #   mutate(X0 = rep(1,nrow(dat))) %>%
  #   relocate(X0, .before = X1) %>%
  #   as.matrix
  
  ### Estimate ###

  if(TypeSetting == "Linear"){
    X = select(dat,-Y) %>%
      mutate(X0 = rep(1,nrow(dat))) %>%
      relocate(X0, .before = X1) %>%
      as.matrix
    ThetaCond = solve(t(X) %*% X) %*% (t(X) %*% mu) %>% as.numeric()
    }else if(TypeSetting == "Logistic"){
      X = select(dat,-Y) %>%
        # mutate(X0 = rep(1,nrow(dat))) %>%
        # relocate(X0, .before = X1) %>%
        as.matrix
      
    
    # Define function to compute log-likelihood
    log_likelihood <- function(theta, X, Y) {
      eta <- theta[1] + X %*% as.matrix(theta[2:(length(theta))])
      p <- 1 / (1 + exp(-eta))
      loglik <- sum(Y * log(p) + (1 - Y) * log(1 - p))
      return(-loglik)  # Minimize negative log-likelihood
    }
    
    # Use numerical optimization to find theta_Cond
    initial_values <- rep(0, ncol(X)+1)  # initial guess for theta
    ThetaCond <- optim(initial_values, log_likelihood, X = X, Y = Y, method = "BFGS")$par
  
  }
  
  return(ThetaCond)
}