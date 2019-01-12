# imports

setwd('R:/Documents/Github/aahps-assignment-5/')

source('tabuSearch.R')
source('readFile.R')
source('subsetCost.R')
source('generateRandomSolution.R')


inputData <- readFile('input1.txt')

U <- inputData[[1]]
S <- inputData[[2]]

objectiveFunction <- function (s) {
  counts <- calculateCounts(length(U), s, S)
  return (sum(calculateCost(U, counts, s, S)))
  
}

ponovitve <- 100
rezultati <- vector(mode="numeric", length=ponovitve)


for(i in 1:ponovitve) {
  s0 <- generateRandomSolution(length(U), S)
  rezTabu <- tabuSearch(U, S, s0, objectiveFunction, maxItter = 1000, tabuSize = 50)
  print(which(rezTabu == 1))
  print(objectiveFunction(rezTabu))
  rezultati[i] = objectiveFunction(rezTabu)
}

print(min(rezultati))