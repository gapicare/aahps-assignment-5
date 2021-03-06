# Change to your path
# setwd("Documents/faks/3.letnik/1.semester/rzhp/aahps-assignment-5/")

source('tabuSearch.R')
source('readFile.R')
source('subsetCost.R')
source('generateRandomSolution.R')

# U_costs -> costs of elements
# S_0 -> initial solution
# S -> list of all subsets
localSearch <- function (U_costs, S_0, S) {
  
  # total number of elements
  n_elements <- length(U_costs)
  
  # total number of subsets
  n_subsets <- length(S_0)
  
  sBest <- S_0
  U_counts <- calculateCounts(n_elements, sBest, S)
  currentCost <- calculateCost(U_costs, U_counts, sBest, S)
  
  foundLocalMin <- 0
  
  # iterate until local min is found
  while (!foundLocalMin) {
    
    # generate neighborhood
    neighborhood = generateNeigbourhood(sBest, U_costs, S)
    
    n_neighborhoods <- length(neighborhood)
    costs <- numeric(n_neighborhoods)
    
    # calculate cost for each neighborhood element
    for (i in 1:n_neighborhoods) {
      U_counts <- calculateCounts(length(U_costs), neighborhood[[i]], S)
      costs[i] <- calculateCost(U_costs, U_counts, neighborhood[[i]], S)
    }
    
    # get index of min cost element
    ixMin <- which.min(costs)
    
    # check if its cost is smaller than current cost
    if (costs[ixMin] < currentCost) {
      sBest <- neighborhood[[ixMin]]
      currentCost <- costs[ixMin]
    } else {
      foundLocalMin <- 1
    }
  }
  
  result <- list(currentCost, sBest)
  return (result)
}

# filename <- name of the input file
# numRuns <- number of runs of localSearch algorithm (default = 100)
runLocalSearch <- function(filename, numRuns = 100) {
  
  start_time <- Sys.time()
  
  input <- readFile(filename)
  U_costs <- input[[1]]
  S <- input[[2]]
  
  costs <- numeric(numRuns)
  solutions <- list()
  
  for (i in 1:numRuns) {
    
    S_0 = generateRandomSolution(length(U_costs), S)
    
    result <- localSearch(U_costs, S_0, S)
    costs[i] <- result[[1]]
    solutions[[i]] <- result[[2]]
  }
  
  ixBest <- which.min(costs)
  chosenIndexes <- solutions[[ixBest]] * (1:length(solutions[[ixBest]]))
  chosenIndexes <- chosenIndexes[chosenIndexes != 0]
  
  print(costs[ixBest])
  print(chosenIndexes)
  
  end_time <- Sys.time()
  
  print(end_time - start_time)
  
  result <- list(costs[ixBest], chosenIndexes)
  return (result)
}
