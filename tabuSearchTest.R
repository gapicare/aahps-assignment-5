# imports

source('tabuSearch.R')
source('readFile.R')
source('subsetCost.R')
source('generateRandomSolution.R')


inputData <- readFile('input1.txt')

U <- inputData[[1]]
S <- inputData[[2]]

s0 <- generateRandomSolution(length(U), S)

objectiveFunction <- function (s) {
  return (calculateCost(U, s, length(U), S))
  
}

rezTabu <- tabuSearch(U, S, s0, objectiveFunction)