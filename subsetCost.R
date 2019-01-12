# n <- number of all elements
# S_c <- current solution
# S <- list of all subsets
calculateCounts = function (n, S_c, S) {
  
  counts <- integer(n)
  
  for (i in 1:length(S_c)) {
    if (S_c[i]) {
      for (ix in S[[i]]) {
        counts[ix] <- counts[ix] + 1
      }
    }
  }
  
  return (counts)
}

# U_costs <- costs of elements
# U_counts <- how many times each element was selected
# S_c <- current solution
# S <- list of all subsets
calculateCost = function (U_costs, U_counts, S_c, S) {
  
  cost <- 0
  n_subsets <- length(S)
  
  for (i in 1:n_subsets) {
    if (S_c[i]) {
      cost_i <- subsetCost(U_costs, U_counts, S[i])
      cost <- cost + cost_i
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
  }
  
  return (cost)
}
