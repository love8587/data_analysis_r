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
  
  data = data[data$State == state,]
  
  if (outcome == 'heart attack') {
    idx <- 11    
    # clean up data
    t_data <- data[!is.na(data[,idx]),]
    return(t_data[which.min(t_data[,idx]),2])
    
 }else if (outcome == 'heart failure') {
   idx <- 17
   # clean up data
   t_data <- data[!is.na(data[,idx]),]
   return(t_data[which.min(t_data[,idx]),2])
 }else
   idx <- 23
   # clean up data
   t_data <- data[!is.na(data[,idx]),]
   return(t_data[which.min(t_data[,idx]),2])
}

