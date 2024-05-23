SimulationCaseFunction = function(delta, gamma, N, rho, K){
  
  ### Small Sample ###
  if(delta == 0 & gamma == 0.0 & N == 50  & K == 1 & rho == 0.0){SimulationCase = 1}
  if(delta == 0 & gamma == 0.0 & N == 50  & K == 5 & rho == 0.0){SimulationCase = 2}
  if(delta == 0 & gamma == 0.0 & N == 50  & K == 1 & rho == 0.1){SimulationCase = 3}
  if(delta == 0 & gamma == 0.0 & N == 50  & K == 5 & rho == 0.1){SimulationCase = 4}
  if(delta == 0 & gamma == 0.0 & N == 200 & K == 1 & rho == 0.0){SimulationCase = 5}
  if(delta == 0 & gamma == 0.0 & N == 200 & K == 5 & rho == 0.0){SimulationCase = 6}
  if(delta == 0 & gamma == 0.0 & N == 200 & K == 1 & rho == 0.1){SimulationCase = 7}
  if(delta == 0 & gamma == 0.0 & N == 200 & K == 5 & rho == 0.1){SimulationCase = 8}
  if(delta == 0 & gamma == 0.5 & N == 50  & K == 1 & rho == 0.0){SimulationCase = 9}
  if(delta == 0 & gamma == 0.5 & N == 50  & K == 5 & rho == 0.0){SimulationCase = 10}
  if(delta == 0 & gamma == 0.5 & N == 50  & K == 1 & rho == 0.1){SimulationCase = 11}
  if(delta == 0 & gamma == 0.5 & N == 50  & K == 5 & rho == 0.1){SimulationCase = 12}
  if(delta == 0 & gamma == 0.5 & N == 200 & K == 1 & rho == 0.0){SimulationCase = 13}
  if(delta == 0 & gamma == 0.5 & N == 200 & K == 5 & rho == 0.0){SimulationCase = 14}
  if(delta == 0 & gamma == 0.5 & N == 200 & K == 1 & rho == 0.1){SimulationCase = 15}
  if(delta == 0 & gamma == 0.5 & N == 200 & K == 5 & rho == 0.1){SimulationCase = 16}
  if(delta == 1 & gamma == 0.0 & N == 50  & K == 1 & rho == 0.0){SimulationCase = 17}
  if(delta == 1 & gamma == 0.0 & N == 50  & K == 5 & rho == 0.0){SimulationCase = 18}
  if(delta == 1 & gamma == 0.0 & N == 50  & K == 1 & rho == 0.1){SimulationCase = 19}
  if(delta == 1 & gamma == 0.0 & N == 50  & K == 5 & rho == 0.1){SimulationCase = 20}
  if(delta == 1 & gamma == 0.0 & N == 200 & K == 1 & rho == 0.0){SimulationCase = 21}
  if(delta == 1 & gamma == 0.0 & N == 200 & K == 5 & rho == 0.0){SimulationCase = 22}
  if(delta == 1 & gamma == 0.0 & N == 200 & K == 1 & rho == 0.1){SimulationCase = 23}
  if(delta == 1 & gamma == 0.0 & N == 200 & K == 5 & rho == 0.1){SimulationCase = 24}
  if(delta == 1 & gamma == 0.5 & N == 50  & K == 1 & rho == 0.0){SimulationCase = 25}
  if(delta == 1 & gamma == 0.5 & N == 50  & K == 5 & rho == 0.0){SimulationCase = 26}
  if(delta == 1 & gamma == 0.5 & N == 50  & K == 1 & rho == 0.1){SimulationCase = 27}
  if(delta == 1 & gamma == 0.5 & N == 50  & K == 5 & rho == 0.1){SimulationCase = 28}
  if(delta == 1 & gamma == 0.5 & N == 200 & K == 1 & rho == 0.0){SimulationCase = 29}
  if(delta == 1 & gamma == 0.5 & N == 200 & K == 5 & rho == 0.0){SimulationCase = 30}
  if(delta == 1 & gamma == 0.5 & N == 200 & K == 1 & rho == 0.1){SimulationCase = 31}
  if(delta == 1 & gamma == 0.5 & N == 200 & K == 5 & rho == 0.1){SimulationCase = 32}
  
  ### Large Sample ###
  if(delta == 0 & gamma == 0.0 & N == 500  & K == 1 & rho == 0.0){SimulationCase = 101}
  if(delta == 0 & gamma == 0.0 & N == 500  & K == 5 & rho == 0.0){SimulationCase = 102}
  if(delta == 0 & gamma == 0.0 & N == 500  & K == 1 & rho == 0.1){SimulationCase = 103}
  if(delta == 0 & gamma == 0.0 & N == 500  & K == 5 & rho == 0.1){SimulationCase = 104}
  if(delta == 0 & gamma == 0.0 & N == 2000 & K == 1 & rho == 0.0){SimulationCase = 105}
  if(delta == 0 & gamma == 0.0 & N == 2000 & K == 5 & rho == 0.0){SimulationCase = 106}
  if(delta == 0 & gamma == 0.0 & N == 2000 & K == 1 & rho == 0.1){SimulationCase = 107}
  if(delta == 0 & gamma == 0.0 & N == 2000 & K == 5 & rho == 0.1){SimulationCase = 108}
  if(delta == 0 & gamma == 0.5 & N == 500  & K == 1 & rho == 0.0){SimulationCase = 109}
  if(delta == 0 & gamma == 0.5 & N == 500  & K == 5 & rho == 0.0){SimulationCase = 1010}
  if(delta == 0 & gamma == 0.5 & N == 500  & K == 1 & rho == 0.1){SimulationCase = 1011}
  if(delta == 0 & gamma == 0.5 & N == 500  & K == 5 & rho == 0.1){SimulationCase = 1012}
  if(delta == 0 & gamma == 0.5 & N == 2000 & K == 1 & rho == 0.0){SimulationCase = 1013}
  if(delta == 0 & gamma == 0.5 & N == 2000 & K == 5 & rho == 0.0){SimulationCase = 1014}
  if(delta == 0 & gamma == 0.5 & N == 2000 & K == 1 & rho == 0.1){SimulationCase = 1015}
  if(delta == 0 & gamma == 0.5 & N == 2000 & K == 5 & rho == 0.1){SimulationCase = 1016}
  if(delta == 1 & gamma == 0.0 & N == 500  & K == 1 & rho == 0.0){SimulationCase = 1017}
  if(delta == 1 & gamma == 0.0 & N == 500  & K == 5 & rho == 0.0){SimulationCase = 1018}
  if(delta == 1 & gamma == 0.0 & N == 500  & K == 1 & rho == 0.1){SimulationCase = 1019}
  if(delta == 1 & gamma == 0.0 & N == 500  & K == 5 & rho == 0.1){SimulationCase = 1020}
  if(delta == 1 & gamma == 0.0 & N == 2000 & K == 1 & rho == 0.0){SimulationCase = 1021}
  if(delta == 1 & gamma == 0.0 & N == 2000 & K == 5 & rho == 0.0){SimulationCase = 1022}
  if(delta == 1 & gamma == 0.0 & N == 2000 & K == 1 & rho == 0.1){SimulationCase = 1023}
  if(delta == 1 & gamma == 0.0 & N == 2000 & K == 5 & rho == 0.1){SimulationCase = 1024}
  if(delta == 1 & gamma == 0.5 & N == 500  & K == 1 & rho == 0.0){SimulationCase = 1025}
  if(delta == 1 & gamma == 0.5 & N == 500  & K == 5 & rho == 0.0){SimulationCase = 1026}
  if(delta == 1 & gamma == 0.5 & N == 500  & K == 1 & rho == 0.1){SimulationCase = 1027}
  if(delta == 1 & gamma == 0.5 & N == 500  & K == 5 & rho == 0.1){SimulationCase = 1028}
  if(delta == 1 & gamma == 0.5 & N == 2000 & K == 1 & rho == 0.0){SimulationCase = 1029}
  if(delta == 1 & gamma == 0.5 & N == 2000 & K == 5 & rho == 0.0){SimulationCase = 1030}
  if(delta == 1 & gamma == 0.5 & N == 2000 & K == 1 & rho == 0.1){SimulationCase = 1031}
  if(delta == 1 & gamma == 0.5 & N == 2000 & K == 5 & rho == 0.1){SimulationCase = 1032}

  return(SimulationCase)
}