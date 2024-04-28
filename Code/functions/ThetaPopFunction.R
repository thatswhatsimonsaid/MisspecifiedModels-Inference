ThetaPopFunction = function(dat, SimulationCase){
  
  ### Summary: Calculates the population regression estimate 
      # according to Abadie, Imbens, Zheng (2014)
  ### Input: 
  # dat: Data set. Particularly, we only need covariates X
  # delta: Misspecification parameter
  # rho: Leverage parameter
  ### Output:
  # Population regression estimate
  

  ### ThetaPop ###
  
  ## Cases 1 - 18 ##
  if(SimulationCase <= 18){ThetaPop = 1.0}
  if(SimulationCase %in% c(21,22,25,26,29,30)){ThetaPop = 1.0}
  if(SimulationCase %in% c(19,20,27,28)){ThetaPop = 1.5}
  if(SimulationCase %in% c(23,24,31,32)){ThetaPop = 1.6}

  return(ThetaPop)
}
