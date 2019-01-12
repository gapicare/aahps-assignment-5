library('dequer')

# U -> Vector of elements cost (element are from 1 to n)
# S -> Subsets of elements in U
# s0 -> Starting sequence (vector of zeros and ones that represent which if sequence has this subeset)
# max_itter -> max itteration if solution was not found before
# objective function
# tabuSize -> Size of tabu list
tabuSearch <- function(U, S, s0, objectiveFunction, maxItter = 100000, tabuSize = 1000){
  tabuList <- queue()
  sBest <- s0
  s <- s0
  for(itt in 1:maxItter) {
    bestCandidate <- s
    for (candindate in generateNeigbourhood(s)){
      if (!elementInList(candindate, tabuList) && objectiveFunction(candindate) > objectiveFunction(bestCandidate)) {
        bestCandidate <- candindate
      }
      
    }
    
    s <- bestCandidate
    
    if (stoppingCriteria(sBestCandidate,sBest, objectiveFunction)) {
      break
    }
    
    
    if (objectiveFunction(bestCandidate) > objectiveFunction(sBest)) {
      sBest <- bestCandidate
    }
    
    
    pushback(tabuList, bestCandidate);
    if (length(tabuList) > tabuSize) {
      pop(tabuList)
    }
    
  }
  
  return (sBest)
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

# L -> list of vectors
elementInList <- function(element, L) {
  for (el in L){
    if (length(element) == length(el) && all(element == el)){
      return(TRUE)
    }
  }
  return (FALSE)
}