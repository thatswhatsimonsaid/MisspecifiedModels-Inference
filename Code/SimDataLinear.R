SimDataLinear = function(N, rho, K, delta, gamma){
  ### Summary: Simulates linear data according to the parameters of 
      # Abadie, Imbens, and Zhang (2014)
  ### Inputs:
    # N: Number of observations
    # rho: Mixture/leverage parameter
    # K: Number of covariates. Note the first covariate will always be 
        # a mixture of a standard normal and log normal(0,0.5^2)
    # delta: Misspecification parameter
    # gamma: Heterogeneity parameter
  ### Output:
    # dat: A data set generated according to the parameters inputted in the 
    #      linear setting.
    # mu: The conditional expectation of Y given X
  
  ### Covariates ###
  X1 = sample(c(rnorm(N*(1-rho), mean = 0, sd = 1),
                rlnorm(N*rho, meanlog = 0, sdlog = sqrt(0.5))))
  X2K = sapply(1:(K-1), function(x) rnorm(n = N, mean = 0, sd = 1))
  
  ### Response ###
  mu_i = X1 + delta * (X1^2 - 1)
  sigma_i = exp(1-gamma*X1)
  Y = rnorm(n = N, mean = mu_i, sd = sigma_i)

  ### Return ###
  if(K == 1){dat = data.frame(Y, X1)}else if(K>1){dat = data.frame(Y, X1, X2K)}
  colnames(dat) = c("Y",paste0("X", 1:K))
  return(list(dat = dat,
              mu = mu_i))
}