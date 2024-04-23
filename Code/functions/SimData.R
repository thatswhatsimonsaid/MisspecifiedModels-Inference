SimData = function(N, rho, K, delta, gamma, Binary = FALSE){
  
  ### Summary: Simulates Data according to the parameters of 
      # Abadie, Imbens, and Zhang (2014)
  ### Inputs:
    # N: Number of observations
    # rho: Mixture/leverage parameter
    # K: Number of covariates. Note the first covariate will always be 
        # a mixture of a standard normal and log normal(0,0.5^2)
    # delta: Misspecification parameter
    # gamma: Heterogeneity parameter
  ### Output:
    # dat: A data set
  
  ### Covariates ###
  # X1 = sample(c(rnorm(N*(1-rho), mean = 0, sd = 1),
  #               rlnorm(N*rho, meanlog = 0, sdlog = sqrt(0.5))))
  
  if(rho == 0){X1 = rnorm(N, mean = 0, sd = 1)}else if(rho !=0){
    MixtureFunction = r(distr::UnivarMixingDistribution(Norm(mean = 0, sd = 1),
                                                        Lnorm(meanlog = 0, 
                                                              sdlog = sqrt(0.5)),
                                                        mixCoeff = c(1-rho, rho)))
    X1 = MixtureFunction(N)
  }
  X2K = sapply(1:(K-1), function(x) rnorm(n = N, mean = 0, sd = 1))
  
  ### Response ###
  mu_i = X1 + delta *(X1^2 - 1)
  sigma_i = sqrt(exp(1 - gamma*X1))
  Ystar = rnorm(n = N, mean = mu_i, sd = sigma_i)
  if(Binary == FALSE){Y = Ystar}else if(Binary == TRUE){Y = 1*(Ystar>=0)}
  
  ### Return ###
  if(K == 1){dat = data.frame(Y, X1)}else if(K>1){dat = data.frame(Y, X1, X2K)}
  colnames(dat) = c("Y",paste0("X", 1:K))
  return(list(dat = dat,
              mu = mu_i))
}