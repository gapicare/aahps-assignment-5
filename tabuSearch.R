#library('dequer')

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
    bestCandidate <- 1:length(S)
    candidates <- generateNeigbourhood(s, U, S)
    #print(candidates)
    for (i in 1:length(candidates) ){
      candindate = candidates[[i]]
      if (!elementInList(candindate, tabuList) && objectiveFunction(candindate) < objectiveFunction(bestCandidate)) {
        bestCandidate <- candindate
      }
      
    }
    

    if (stoppingCriteria(bestCandidate,s, objectiveFunction)) {
      print('Stopped after:')
      print(itt)
      
      break
    }
    s <- bestCandidate
    
    
    if (objectiveFunction(bestCandidate) < objectiveFunction(sBest)) {
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
# U -> All elements
generateNeigbourhood <- function (s, U, S) {
  neigbourhood <- list()
  j = 1
  for (i in 1:length(s)) {
    temp <- s
    temp[i] <- abs(temp[i] - 1)
    if (checkIfValidSubset(temp, U, S)) {
      neigbourhood[[j]] <- temp
      j <- j + 1
    }
  }
  
  return (neigbourhood)
}

# L -> list of vectors
elementInList <- function(element, L) {
  lst <- as.list(L)
  for (el in lst){
    if (length(element) == length(el) && all(element == el)){
      return(TRUE)
    }
  }
  return (FALSE)
}


checkIfValidSubset <- function(s, U, S){
  C <- vector(mode="numeric", length=0)  # Already covered items in U
  U_elements <- 1:length(U)
  for (i in 1:length(s)){
    if (s[i] == 1) {
      C <- union(C, S[[i]])
    }
  }
  C <- sort(C)
  return(all(C == U_elements))
  
}
