# return data ranking rate by state

rankall <- function(outcome,num = "best"){
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  str_outcome <- c("heart attack","heart failure","pneumonia")
  
  if (!outcome %in% str_outcome){ stop("invalid outcome") }
  
  # heart attack
  data[,11] <- suppressWarnings(as.numeric(data[,11]))
  # heart failure
  data[,17] <- suppressWarnings(as.numeric(data[,17]))
  # pneumonia
  data[,23] <- suppressWarnings(as.numeric(data[,23]))
  
  # clean up NA
  # cc <- complete.cases(data)
  # data <- data[cc,]
  
  # outcome index and outcome t_data
  if (outcome == 'heart attack') {
    idx = 11
    t_data <- data[!is.na(data[,idx]),c(2,7,idx)]
  }else if (outcome == 'heart failure') {
    idx = 17
    t_data <- data[!is.na(data[,idx]),c(2,7,idx)]
  } else {
    idx = 23
    t_data <- data[!is.na(data[,idx]),c(2,7,idx)]
  } 

  names(t_data) <- c("hospital","state","rate")
    
  # 
  rank_val <- tapply(t_data[,"rate"],t_data$state,FUN=function(x,num) {
    
    if (num == "best") {
      #print(which.min(x))
      return (x[which.min(x)])
      
    }else if (num == "worst") {
      
      #print(which.max(x))
      return (x[which.max(x)])
      
    }else {
      
      rank = as.integer(num)
      
      if(is.na(rank) || rank > length(x)) {
        return (NA)
      }
      
      idx = order(x)[rank]
      return (x[idx])
    }},num)
  
  # extract hospital name base on rating
  
  
  states <- names(rank_val)
  ret = data.frame()
  
  for (i in 1:length(states)) 
  {
    st <- states[i]
    rate <- rank_val[[i]]
    
    if (num == "best" || num == "worst") {
        ret <- rbind(ret, t_data[t_data$state == st & t_data$rate == rate,])
    
    }else{
      
      if(!is.na(rate)){
        
        tmp <- t_data[t_data$state==st & t_data$rate <= rate,]
        ret <- rbind(ret, tmp[order(tmp$rate,tmp$hospital),])
      }else{
        #print(st)
        #ret <-rbind(ret,list("hospital"=NA,"state"=st,"rate"=rate))
      }
    }
    
  } # for
  return(ret[,c(1,2)])

}