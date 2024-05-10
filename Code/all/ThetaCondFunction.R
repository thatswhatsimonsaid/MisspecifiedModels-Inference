ThetaCondFunction = function(dat, mu){
  
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
  # Y = dat$Y
  X = select(dat,-Y) %>%
    mutate(X0 = rep(1,nrow(dat))) %>%
    relocate(X0, .before = X1) %>%
    as.matrix
  
  ### Estimate ###
  ThetaCond = solve(t(X) %*% X) %*% (t(X) %*% mu) %>% as.numeric()
  
  return(ThetaCond)
}