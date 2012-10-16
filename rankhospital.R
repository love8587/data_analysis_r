rankhospital <-function(state,outcome,num = "best"){
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  if (is.na(match(state,data$State)))  stop("Invalid state") 
    
  str_outcome <- c("heart attack","heart failure","pneumonia")
  
  if (!outcome %in% str_outcome){ stop("invalid outcome")  }
    
  
  # heart attack
  data[,11] <- suppressWarnings(as.numeric(data[,11]))
  # heart failure
  data[,17] <- suppressWarnings(as.numeric(data[,17]))
  # pneumonia
  data[,23] <- suppressWarnings(as.numeric(data[,23]))
  # state
  data$State <- factor(data$State)
  
  # clean up NA
  cc <- complete.cases(data)
  data <- data[cc,]
  
  # data for the state
  t_data = data[data$State == state,]
  
  check_rank <- function(num,data) {
    
    max = which.max(data)
    if (num == "best") {
      
      return(which.min(data))
    }else if (num == 'worst'){
      return(max)
    }
    
    rank = as.integer(num)
    
    if (rank > max)
      return(NA)
    return(rank)
  }
  
  if (outcome == 'heart attack') {
    t_data <- t_data[with(t_data,order(t_data[,11])),]
    rank = check_rank(num,t_data[,11])
    
  }else if (outcome == 'heart failure') {
    t_data <- t_data[with(t_data,order(t_data[,17])),]
    rank = check_rank(num,t_data[,17])
    
  }else{
    t_data <- t_data[with(t_data,order(t_data[,23])),]
    rank = check_rank(num,t_data[,23])

  }

  #print(rank)
  
  if(is.integer(rank))
    return( t_data[rank,2])
  else
    return(rank)

}