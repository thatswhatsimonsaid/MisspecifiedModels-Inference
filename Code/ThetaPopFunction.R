ThetaPopFunction = function(dat, rho, K, delta, gamma, TypeSetting){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  

  
  ### ThetaPop ###
  
  for(i in 1:nrow(ParameterVector)){
    delta = ParameterVector$MisspecVec[i]
    gamma = ParameterVector$HomoskedVec[i]
    N = ParameterVector$SizeVec[i]
    rho = ParameterVector$LeverageVec[i]
    K = ParameterVector$KVec[i]
  
  if(delta==0 & gamma==0 & N==50 & rho==0 & K==1){ThetaPop =1} #1
  if(delta==0 & gamma==0 & N==50 & rho==0 & K==5){ThetaPop =1} #2
  if(delta==0 & gamma==0 & N==50 & rho==0.1 & K==1){ThetaPop =1} #3
  if(delta==0 & gamma==0 & N==50 & rho==0.1 & K==5){ThetaPop =1} #4
  if(delta==0 & gamma==0 & N==200 & rho==0 & K==1){ThetaPop =1} #5
  if(delta==0 & gamma==0 & N==200 & rho==0 & K==5){ThetaPop =1} #6
  if(delta==0 & gamma==0 & N==200 & rho==0.1 & K==1){ThetaPop =1} #7
  if(delta==0 & gamma==0 & N==200 & rho==0.1 & K==5){ThetaPop =1} #8
  if(delta==0 & gamma==0.5 & N==50 & rho==0 & K==1){ThetaPop =1} #9
  if(delta==0 & gamma==0.5 & N==50 & rho==0 & K==5){ThetaPop =1} #10
  if(delta==0 & gamma==0.5 & N==50 & rho==0.1 & K==1){ThetaPop =1} #11
  if(delta==0 & gamma==0.5 & N==50 & rho==0.1 & K==5){ThetaPop =1} #12
  if(delta==0 & gamma==0.5 & N==200 & rho==0 & K==1){ThetaPop =1} #13
  if(delta==0 & gamma==0.5 & N==200 & rho==0 & K==5){ThetaPop =1} #14
  if(delta==0 & gamma==0.5 & N==200 & rho==0.1 & K==1){ThetaPop =1} #15
  if(delta==0 & gamma==0.5 & N==200 & rho==0.1 & K==5){ThetaPop =1} #16
  if(delta==1 & gamma==0 & N==50 & rho==0 & K==1){ThetaPop =1} #17
  if(delta==1 & gamma==0 & N==50 & rho==0 & K==5){ThetaPop =1} #18
  if(delta==1 & gamma==0 & N==50 & rho==0.1 & K==1){ThetaPop =1.6} #19
  if(delta==1 & gamma==0 & N==50 & rho==0.1 & K==5){ThetaPop =1.6} #20
  if(delta==1 & gamma==0 & N==200 & rho==0 & K==1){ThetaPop =1} #21
  if(delta==1 & gamma==0 & N==200 & rho==0 & K==5){ThetaPop =1} #22
  if(delta==1 & gamma==0 & N==200 & rho==0.1 & K==1){ThetaPop =1.7} #23
  if(delta==1 & gamma==0 & N==200 & rho==0.1 & K==5){ThetaPop =1.7} #24
  if(delta==1 & gamma==0.5 & N==50 & rho==0 & K==1){ThetaPop =1} #25
  if(delta==1 & gamma==0.5 & N==50 & rho==0 & K==5){ThetaPop =1} #26
  if(delta==1 & gamma==0.5 & N==50 & rho==0.1 & K==1){ThetaPop =1.6} #27
  if(delta==1 & gamma==0.5 & N==50 & rho==0.1 & K==5){ThetaPop =1.6} #28
  if(delta==1 & gamma==0.5 & N==200 & rho==0 & K==1){ThetaPop =1} #29
  if(delta==1 & gamma==0.5 & N==200 & rho==0 & K==5){ThetaPop =1} #30
  if(delta==1 & gamma==0.5 & N==200 & rho==0.1 & K==1){ThetaPop =1.7} #31
  if(delta==1 & gamma==0.5 & N==200 & rho==0.1 & K==5){ThetaPop =1.7} #32

   print(paste0("Case ", i, ": ",ThetaPop))
  }
  return(ThetaPop)
}
