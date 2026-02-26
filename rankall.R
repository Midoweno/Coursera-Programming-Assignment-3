##LAPCDIR
##setwd("C:/Users/guore/Downloads/rprog_data_ProgAssignment3-data")

##mac dir
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
  
  
  ##Lookup vector created (converts each string input to numbers to index columns)
  outcome_columns<-c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  colnum <- outcome_columns[outcome]
  
  
  ## Checking that outcome is valid, if invalid, stop
  if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    #Checking that outcome is one of the 3 given outcomes, now continue
      stop("invalid outcome")
  }
  ##continue below
  
  
  ##Ordering based on outcome
  df <- dfalloutcomes[
    order(dfalloutcomes[,7],
          dfalloutcomes[,colnum],
          dfalloutcomes[,2]),
  ]
  state_list<-split(df,df$State) ##split into states
  rows_per_state<-sapply(state_list,nrow)
  
  
  ##Now for "num" input, extract to a new df depending on input
  if (num == "best"){
      output<-do.call(rbind,lapply(state_list,function(x){x[1,]}))
      output<-output[,c(2,7)]
      output
  } else if (num=="worst"){
      output<-do.call(rbind,lapply(state_list,function(x){x[nrow(x),]}))
      output<-output[,c(2,7)]
      output
  } else if(any(num>rows_per_state)) {
    stop(NA)##### LEFT OFF HERE, CHANGE THIS SO NA REPLACES THE STATE WITH FEWER ROW THAN NUM INPUT
  } else {
    output<-do.call(rbind,lapply(state_list,function(x){x[num,]}))
    output<-output[,c(2,7)]
    output
  }
}