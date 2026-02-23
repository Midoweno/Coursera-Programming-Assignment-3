##setwd("/Users/renoguo/Documents/Coursera/rprog_data_ProgAssignment3-data")
rankall <- function(outcome, num = "best") {
  
  
  ## Read outcome data
  dfalloutcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##heart attack
  dfalloutcomes[, 11] <- as.numeric(dfalloutcomes[, 11])
  ##heart failure
  dfalloutcomes[, 17] <- as.numeric(dfalloutcomes[, 17])
  ##pneumonia
  dfalloutcomes[, 23] <- as.numeric(dfalloutcomes[, 23])
  
  outcome_columns<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  ## Checking that outcome is valid
  if (outcome %in% c("heart attack","heart failure","pneumonia")){
    #Checking that outcome is one of the 3 given outcomes, now continu
    
    
    else{
      stop("invalid outcome")
    }
  }
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}