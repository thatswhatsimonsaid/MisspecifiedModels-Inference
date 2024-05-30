ConfidenceIntervalFunction = function(ThetaHat, ThetaCond, ThetaPop, SEPop, SECond, alpha){
  
  ### Summary: Calculates and returns the four confidence intervals according to Abadie, Imbens, and Zheng (2014)
  ### Inputs:
    # ThetaHat: theta_hat: OLS estimate of theta 
    # ThetaCond: theta_conditional: Conditional estimand of interest
    # ThetaPop: theta_population: Population estimand of interest
    # SEPop: Conditional estimated regression variance
    # SECond: Population estimated regression variance
    # alpha: Confidence level
  ### Output:
    # Four confidence intervals
  
  ### Confidence Intervals ###
  CI_SEPop = ThetaHat + outer(SEPop*qnorm(1-alpha/2, mean = 0, sd = 1), c(-1,1))
  CI_SECond = ThetaHat + outer(SECond*qnorm(1-alpha/2, mean = 0, sd = 1), c(-1,1))
  
  ### Containment ###
  Containment_ThetaPop_SEPop = ThetaPop >= CI_SEPop[1] & ThetaPop <= CI_SEPop[2]
  Containment_ThetaPop_SECond = ThetaPop >= CI_SECond[1] & ThetaPop <= CI_SECond[2]
  Containment_ThetaCond_SEPop = ThetaCond >= CI_SEPop[1] & ThetaCond <= CI_SEPop[2]
  Containment_ThetaCond_SECond = ThetaCond >= CI_SECond[1] & ThetaCond <= CI_SECond[2]

  ### Output ###
  CoverageOutput = matrix(c(Containment_ThetaPop_SEPop, 
                            Containment_ThetaPop_SECond, 
                            Containment_ThetaCond_SEPop, 
                            Containment_ThetaCond_SECond), 
                          ncol = 4)
  colnames(CoverageOutput) = c("Coverage_ThetaPop_SEPop", 
                               "Coverage_ThetaPop_SECond", 
                               "Coverage_ThetaCond_SEPop", 
                               "Coverage_ThetaCond_SECond")
  
  return(CoverageOutput)
}