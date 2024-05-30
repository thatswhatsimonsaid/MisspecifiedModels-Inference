ThetaCondFunction = function(dat, mu, TypeSetting){
  
  ### Summary: Calculates the conditional regression estimate 
      #  according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # mu: the N-vector whose i’th element is equal to µ_{Xi}, 
      # which is the mean of Y_i|X_{1i},...,X_{Ki}
  ### Output:
  # Conditional estimand
  
  ### Set Up ###
  N = nrow(dat)
  Y = dat$Y
  
  ### Estimate ###

  if(TypeSetting == "Linear"){
    X = select(dat,-Y) %>%
      mutate(X0 = rep(1,nrow(dat))) %>%
      relocate(X0, .before = X1) %>%
      as.matrix
    ThetaCond = as.numeric(solve(t(X) %*% X) %*% (t(X) %*% mu))[2] # Linear regression mu ~ X
    }else if(TypeSetting == "Logistic"){

      Formula = as.formula(paste0("mu$mu ~ ", paste0(paste0("dat$X", 1:K), collapse = " + "), collapse = ""))
      ThetaCond = as.numeric(coefficients(lm(Formula))[mu$val])
    }
  
  return(ThetaCond)
}