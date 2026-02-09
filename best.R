##ASSIGNMENT PART 1: Plot the 30-day mortality rates for heart attack

setwd("/Users/renoguo/Documents/Coursera/rprog_data_ProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

best <- function(state, outcome) {
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}