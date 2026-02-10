rankhospital <- function(state, outcome, num = "best") {
  dfalloutcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##heart attack
  dfalloutcomes[, 11] <- as.numeric(dfalloutcomes[, 11])
  ##heart failure
  dfalloutcomes[, 17] <- as.numeric(dfalloutcomes[, 17])
  ##pneumonia
  dfalloutcomes[, 23] <- as.numeric(dfalloutcomes[, 23])

  if (state %in% dfalloutcomes[,7]){
    if (outcome %in% c("heart attack","heart failure","pneumonia")){
      #continue
      statedf <- dfalloutcomes[dfalloutcomes[, 7] == state, ]
      if (outcome=="heart attack"){
        outcomewstate<-statedf[,c(2,11)]
        outcomewstate<-na.omit(outcomewstate)
        df_ordered<-outcomewstate[order(outcomewstate[,2],outcomewstate[,1]),]
        colnames(df_ordered)[2]<-"Rate"
        df_ordered$Rank<-seq_len(nrow(df_ordered))
        if (num=="best"){
          head(df_ordered,1)
        }
        else if (num=="worst"){
          tail(df_ordered,1)
        }
        else if (is.numeric(num)){
          if (num%%1!=0){
            stop("Please enter a whole number!")
          }
          else if (num>nrow(df_ordered)){
          return(NA)
          }
          else{
            df_ordered[df_ordered$Rank == num, ]
          }
        }
        else{
          stop("invalid input. Please enter a whole number, 'worst',or 'best'")
        }
      }
      else if (outcome=="heart failure"){
        outcomewstate<-statedf[,c(2,17)]
        outcomewstate<-na.omit(outcomewstate)
        df_ordered<-outcomewstate[order(outcomewstate[,2],outcomewstate[,1]),]
        colnames(df_ordered)[2]<-"Rate"
        df_ordered$Rank<-seq_len(nrow(df_ordered))
        if (num=="best"){
          head(df_ordered,1)
        }
        else if (num=="worst"){
          tail(df_ordered,1)
        }
        else if (is.numeric(num)){
          if (num%%1!=0){
            stop("Please enter a whole number!")
          }
          else if (num>nrow(df_ordered)){
            return(NA)
          }
          else{
            df_ordered[df_ordered$Rank == num, ]
          }
        }
        else{
          stop("invalid input. Please enter a whole number, 'worst',or 'best'")
        }
      }
      else if (outcome=="pneumonia"){
        outcomewstate<-statedf[,c(2,23)]
        outcomewstate<-na.omit(outcomewstate)
        df_ordered<-outcomewstate[order(outcomewstate[,2],outcomewstate[,1]),]
        colnames(df_ordered)[2]<-"Rate"
        df_ordered$Rank<-seq_len(nrow(df_ordered))
        if (num=="best"){
          head(df_ordered,1)
        }
        else if (num=="worst"){
          tail(df_ordered,1)
        }
        else if (is.numeric(num)){
          if (num%%1!=0){
            stop("Please enter a whole number!")
          }
          else if (num>nrow(df_ordered)){
            return(NA)
          }
          else{
            df_ordered[df_ordered$Rank == num, ]
          }
        }
        else{
          stop("invalid input. Please enter a whole number, 'worst',or 'best'")
        }
      }
    }      
    else{
      stop("invalid outcome")
    }
  }  
  else{
    stop("invalid state")
  }
}