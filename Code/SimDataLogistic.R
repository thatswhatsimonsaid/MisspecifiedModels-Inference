SimDataLogistic = function(N, rho, K, delta, gamma){
  
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
  
  ### Coefficient Matrix ###
  if(K == 1){CoefficientVector = matrix(1)}else if(K>1){
    CoefficientVector = matrix(c(1,rep(0,K-1)))}
  
  ### Covariates ###
  X1 = sample(c(rnorm(N*(1-rho), mean = 0, sd = 1),
                rlnorm(N*rho, meanlog = 0, sdlog = sqrt(0.5))))
  
  X2K = sapply(1:(K-1), function(x) rnorm(n = N, mean = 0, sd = 1))
  if(K == 1){XMatrix = as.numeric(X1)}else if(K>1){
    XMatrix = matrix(cbind(X1, X2K), ncol = K)}
  
  ### Error ###
  epsilon_star_i = rlogis(n = N, location = 0, scale = 1)
  
  ### Heteroskedasticity Gamma ###
  if(gamma == 0){
    epsilon_i = epsilon_star_i
  }else if(gamma !=0){
    epsilon_i = epsilon_star_i*exp(1-gamma*X1)
  }
  
  ### Model Misspecification Delta ###
  if(delta == 0){
    Ystar = rep(0,N) + XMatrix %*% CoefficientVector + epsilon_i
    }else if(delta !=0){
    Ystar = X1 + (X1^2 - 1) + epsilon_i
    }
   
  ### Response ###
  Y = 1*(Ystar>=0)
  
  ### Mu ###
  mu = Ystar
  # mu = 1*((Ystar - sign(epsilon_i)*sqrt(abs(epsilon_i)))>=0)
  # mu = (rep(0,N) + XMatrix %*% CoefficientVector)
  # mu = 1*(Ystar>=0.5)
  # mu = as.numeric(1+ exp(0 + XMatrix %*% CoefficientVector))
  # mu = 1*((1/(as.numeric(1+ exp(0 + XMatrix %*% CoefficientVector))))>=0.5)
  # mu = sign(Ystar)*log(abs(Ystar))
  # mu_goal = solve(t(X), (t(X) %*% X) %*% c(0,1))
  
  # ThetaCondFunction(dat,mu,TypeSetting)
   
  # solve(t(X)) %*% (t(X) %*% X) %*% beta_hat
  
  ### Return ###
  dat = data.frame(Y, XMatrix)
  colnames(dat) = c("Y",paste0("X", 1:K))
  
  return(list(dat = dat,
              mu = mu))
}