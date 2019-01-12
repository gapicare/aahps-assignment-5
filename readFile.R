# filename <- filename of a file to be read
readFile = function (filename) {
  # open a file for reading
  f = file(filename, open = "r")
  
  # read the line of costs
  line = readLines(f, n = 1)
  U_costs = as.numeric(unlist(strsplit(line, split = ", ")))
  
  # read other lines
  S = list();
  i <- 1
  while (TRUE) {
    line = readLines(f, n = 1)
    if (length(line) == 0) {
      break
    }
    values = as.numeric(unlist(strsplit(line, split = ", ")))
    S[[i]] <- values
    i <- i + 1
  }
  close(f)
  
  #return list of U and S
  input = list()
  input[[1]] <- U_costs
  input[[2]] <- S
  return (input)
}
