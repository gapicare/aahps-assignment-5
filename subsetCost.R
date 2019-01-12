subsetCost = function(U_costs, U_counts, S_i) {
  
  cost <- 0
  
  for(ix in S_i) {
    cost_i <- U_costs[ix] * 1.05^(U_counts[ix] - 1)
    cost <- cost + cost_i
  }
  
  cost
}