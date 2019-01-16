# imports

setwd('R:/Documents/Github/aahps-assignment-5/')

source('tabuSearch.R')
source('readFile.R')
source('subsetCost.R')
source('generateRandomSolution.R')


objectiveFunction <- function (s) {
  counts <- calculateCounts(length(U), s, S)
  return (sum(calculateCost(U, counts, s, S)))
  
}

ponovitve <- 10



name_files <- c("input1.txt", "input2.txt", "input3.txt", "input4.txt", "input5.txt", "input6.txt", "input7.txt", "input8.txt", "input9.txt", "input10.txt")
for (file_name in name_files) {
  inputData <- readFile(file_name)
  
  U <- inputData[[1]]
  S <- inputData[[2]]
  rezultati <- vector(mode="numeric", length=ponovitve)
  rez_index <- 0
  min_rez = 100000000000
  
  for(i in 1:ponovitve) {
    s0 <- generateRandomSolution(length(U), S)
    rezTabu <- tabuSearch(U, S, s0, objectiveFunction, maxItter = 1000, tabuSize = 500)
    #print(which(rezTabu == 1))
    #print(objectiveFunction(rezTabu))
    if (objectiveFunction(rezTabu) < min_rez) {
      min_rez <- objectiveFunction(rezTabu)
      rez_index <- which(rezTabu == 1)
    }
  }

  print(min_rez)
  print(rez_index)
}