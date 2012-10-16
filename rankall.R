# return data ranking rate by state

ranks <- function(X,num)  {
  # return ranking rates based on num
  #  - best, worst or number
  #
  if (num == "best"){
    return(min(X))
  } 
  else if (num == "worst") {
    
    #print(X)
    #print(max(X))
    return(max(X))
  }
  else {
    num <- as.integer(num)
    if (!is.na(num) & num <= length(X))
    {
      ord <- order(X)
      return(X[ord[num]])
    }
    return(NA)
  } 
  return(NA)
}

extract_rank_data <- function(x,data,index,num)
{
  # extract rank data based on ranks rating
  # x is list of rank rating
  #
  ret <- data.frame()
  print(index)
  for (i in 1:length(x)) {
  
    st <- names(x[i])
    val <- x[i]
    if (!is.na(val)){
      if (num == 'worst')
        tmp <- data[data$State == st & data[,index]>=val,]
      else
        tmp <- data[data$State == st & data[,index]<=val,]

      tmp <- tmp[,c(2,7)]
      ret <- rbind(ret, tmp)
    }
  }
  return(ret)
}


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
  
  # outcome index
  if (outcome == 'heart attack') {
    idx = 11
    t_data <- data[!is.na(data[,idx]),]
  }else if (outcome == 'heart failure') {
    idx = 17
    t_data <- data[!is.na(data[,idx]),]
  } else {
    idx = 23
    t_data <- data[!is.na(data[,idx]),]
  }  
  # default
  
  rank_val <- tapply(t_data[,idx],t_data$State,ranks,num)
  #print(rank_val)

  ret <- extract_rank_data(rank_val,t_data,idx,num)
  #return(con_list_to_df(ret))
  return(ret)
  
}