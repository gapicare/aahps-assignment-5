# U -> Vector of elements cost (element are from 1 to n)
# S -> Subsets of elements in U
# s0 -> Starting sequence (vector of zeros and ones that represent which if sequence has this subeset)
# max_itter -> max itteration if solution was not found before
# objective function
# tabuSize -> Size of tabu list
tabuSearch <- function(U, S, s0, objectiveFunction, maxItter = 100000, tabuSize = 1000){
  tabuList <- list()
  sBest <- s0
  s <- s0
  for(itt in 1:maxItter) {
    
    for (candindate in generateNeigbourhood(s)){
      #TODO:
    }
    
    
    
    if (stoppingCriteria(sBestCandidate,sBest, objectiveFunction)) {
      break
    }
    
  }
  
}


stoppingCriteria <- function (S1, S2, objectiveFunction, epsilon = 1e-12) {
  if (abs(objectiveFunction(S2) - objectiveFunction(S1)) < epsilon) {
    return(TRUE)
  }
  
  return(FALSE)
  
}

# s -> Current list of subset
# S -> All possible subsets
generateNeigbourhood <- function (s) {
  neigbourhood <- list()
  
  for (i in 1:length(s)) {
    temp <- s
    temp[i] <- abs(temp[i] - 1)
    neigbourhood[[i]] <- temp
  }
  
  return (neigbourhood)
}