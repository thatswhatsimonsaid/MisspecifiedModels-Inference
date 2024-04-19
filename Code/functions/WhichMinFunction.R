WhichMinFunction = function(DistanceMatrix, k){
  ### Summary: Returns the indices of the k'th lowest point of each of column
  ### Inputs:
    # DistanceMatrix: A distance matrix
    # k: the k'th lowest. For the min, set k = 1.
  ### Output:
    # MinIndices: The indices of the k'th lowest point
  
  MinVal = apply(X = DistanceMatrix, MARGIN = 1, FUN = function(x) sort(x, decreasing = FALSE)[k])
  MinIndices = sapply(1:ncol(DistanceMatrix), function(ColIndices){which(DistanceMatrix[,ColIndices] == MinVal[ColIndices])})
  return(MinIndices)
}