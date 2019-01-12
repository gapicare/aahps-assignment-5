# U_costs -> costs of elements
# S_0 -> initial solution
# S -> list of all subsets
localSearch = function (U_costs, S_0, S) {
  
  n_subsets <- length(S_0)
  costs <- integer(n_subsets)
  sBest <- S_0
  
  currentCost <- calculateCost(U_costs, sBest, S)
  foundLocalMin <- 0
  
  # iterate until local min is found
  while (!foundLocalMin) {
    
    # generate neighborhood
    neighborhood = generateNeighborhood(sBest)
    
    # calculate cost for each neighborhood element
    for (i in 1:n_subsets) {
      costs[i] <- calculateCost(U_costs, neighborhood[[i]], S)
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
  
  
}
