# U_costs <- costs of elements
# S_0 <- initial solution
# S <- list of all subsets
# T <- temperature
# lambda <- lambda factor
# maxIter <- maximum number of iterations
simulatedAnnealing <- function (U_costs, S_0, S, T = 100, lambda = 0.95, maxIter = 10) {
  
  set.seed(1)
  n_elements <- length(U_costs)
  
  S_temp <- S_0
  S_best <- S_0
  
  U_counts <- calculateCounts(n_elements, S_best, S)
  bestCost <- calculateCost(U_costs, U_counts, S_best, S)
  
  U_counts <- calculateCounts(n_elements, S_best, S)
  tempCost <- calculateCost(U_costs, U_counts, S_best, S)
 
  iter <- 0
  while (iter < maxIter) {
    
    neighborhood <- generateNeigbourhood(S_temp, U_costs, S)
    randIx <- sample(1: length(neighborhood), 1)
    S_rand <- neighborhood[[randIx]]
    
    U_counts <- calculateCounts(n_elements, S_rand, S)
    randCost <- calculateCost(U_costs, U_counts, S_rand, S)
    
    if (randCost < bestCost) {
      S_best <- S_rand
      bestCost <- randCost
    }
    
    if (randCost < tempCost) {
      S_temp <- S_rand
      tempCost <- randCost
    } else {
      p <- exp(-(randCost - tempCost))
      makeMove <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(1-p, p))
      if (makeMove) {
        S_temp <- S_rand
        tempCost <- randCost
      }
      T <- T * lambda
    }
    iter <- iter + 1
  }
  
  return (localSearch(U_costs, S_best, S))
}

# filename <- name of input file
# numRuns <- number of runs of simmulatedAnnealing algorithm
runSimulatedAnnealing <- function (filename, numRuns = 20) {
  
  start_time <- Sys.time()
  
  input <- readFile(filename)
  U_costs <- input[[1]]
  S <- input[[2]]
  
  costs <- numeric(numRuns)
  solutions <- list()
  
  for (i in 1:numRuns) {
    
    S_0 = generateRandomSolution(length(U_costs), S)
    
    result <- simulatedAnnealing(U_costs, S_0, S)
    costs[i] <- result[[1]]
    solutions[[i]] <- result[[2]]
  }
  
  ixBest <- which.min(costs)
  
  print(costs[ixBest])
  
  end_time <- Sys.time()
  
  print(end_time - start_time)
  #bestSolution = list()
  #bestSolution[[1]] <- costs[ixBest]
  #bestSolution[[2]] < solutions[[ixBest]]
  #return (bestSolution)
  
  return (costs[ixBest])
}
