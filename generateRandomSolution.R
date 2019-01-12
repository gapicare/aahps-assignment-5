# n - number of elements, S - list of subsets
generateRandomSolution = function(n, S) {
  
  n_subsets <- length(S)
  
  U <- integer(n)
  added <- integer(n_subsets)
  
  # while not all elements are used
  # add a random subset
  while(sum(U) != n) {
    # choose a random subset
    i <- sample(1:n_subsets, 1)
    
    # check if you have not used it before
    if(!added[i]) {
      # mark it as used
      added[i] <- 1
      
      #mark all elements in this subset as used
      for(ix in S[[i]]) {
        U[i] <- 1
      }
    }
  }
  
  # added has 1's at indices of subsets that were chosen
  # and 0's elswere
  added
}