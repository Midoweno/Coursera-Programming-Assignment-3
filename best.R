##ASSIGNMENT PART 1: Plot the 30-day mortality rates for heart attack
##MAC directory
##setwd("/Users/renoguo/Documents/Coursera/rprog_data_ProgAssignment3-data")

##PC directory
setwd("C:/Users/guore/Downloads/rprog_data_ProgAssignment3-data")


##Assignment directions include this part of the code
##outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
##outcome[, 11] <- as.numeric(outcome[, 11])

##hist(outcome[, 11])

best <- function(state, outcome) {
  dfalloutcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##heart attack
  dfalloutcomes[, 11] <- as.numeric(dfalloutcomes[, 11])
  ##heart failure
  dfalloutcomes[, 17] <- as.numeric(dfalloutcomes[, 17])
  ##pneumonia
  dfalloutcomes[, 23] <- as.numeric(dfalloutcomes[, 23])
  
  ## Check that state and outcome are valid
  if (state %in% dfalloutcomes[,7]){
    if (outcome %in% c("heart attack","heart failure","pneumonia")){
      #continue
      statedf <- dfalloutcomes[dfalloutcomes[, 7] == state, ]
      ##insert if loop here that sorts out the outcomes
      else{
        stop("invalid outcome")
      }
    }
  else{
    stop("invalid state")
  }
  ## Read outcome data

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  }
}