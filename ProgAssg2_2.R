best <-function(state,outcome) {
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  str_states <- data$State
  if (!state %in% str_states){
    stop("Invalid state")
  }
  
  str_outcome <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% str_outcome){

    stop("invalid outcome")
  }
  
  # 
  data[,11] <- suppressWarnings(as.numeric(data[,11]))
  data[,17] <- suppressWarnings(as.numeric(data[,17]))
  data[,23] <- suppressWarnings(as.numeric(data[,23]))
  data$State <- factor(data$State)
  
  cc <- complete.cases(data)
  mydata <- data[cc,]
  t_data =mydata[mydata$State == state,]
  
  if (outcome == 'heart attack') {
    return(t_data[which.min(t_data[,11]),2])
 }else if (outcome == 'heart failure') {
   return(t_data[which.min(t_data[,17]),2])
   
 }else
    return(t_data[which.min(t_data[,23]),2])
}

