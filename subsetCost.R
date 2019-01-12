# U_costs <- costs of elements
# S_c <- current solution
# n <- number of all items
# S <- list of all subsets
calculateCost = function (U_costs, S_c, n, S) {
  
  cost <- 0
  n_subsets <- length(S)
  U_counts <- integer(n)
  
  for (i in 1:n_subsets) {
    if (S_c[i]) {
      ret <- subsetCost(U_costs, U_counts, S[i])
      cost <- cost + ret[[1]]
      U_counts <- ret[[2]]
    }
  }
  
  return (cost)
}

# U_costs <- costs of elements
# U_counts <- how many times each element was selected
# S_i <- subset i
subsetCost = function (U_costs, U_counts, S_i) {
  
  cost <- 0
  
  for (ix in S_i) {
    cost_i <- U_costs[ix] * 1.05^(U_counts[ix] - 1)
    cost <- cost + cost_i
    U_counts[ix] <- U_counts[ix] + 1
  }
  
  ret = list()
  ret[[1]] <- cost
  ret[[2]] <- U_counts
  
  return (ret)
}