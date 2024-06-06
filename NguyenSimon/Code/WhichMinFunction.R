WhichMinFunction = function(DistanceMatrix, k){
  ### Summary: Finds the nearest neighbor of observation i. That is, it returns
  #            the index/indices of the k'th nearest neighbor of observation i.
  ### Inputs:
    # DistanceMatrix: A distance matrix.
    # k: the k'th lowest. For the min, set k = 1.
  ### Output:
    # MinIndices: The indices of the k'th lowest point.
  
  MinVal = apply(X = DistanceMatrix, 
                 MARGIN = 1, 
                 FUN = function(x) sort(x, decreasing = FALSE)[k])
  sapply(1:ncol(DistanceMatrix), 
         function(ColIndices){
           which(DistanceMatrix[,ColIndices] == MinVal[ColIndices])
           }) -> MinIndices
  return(MinIndices)
}