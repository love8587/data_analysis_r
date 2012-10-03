getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  
  fn <- dir(directory)
  idx <- as.numeric(substr(fn,1,3))
  fn <- paste(directory,"/",fn,sep="")
  #
  names(fn) <- idx
  
  dat <- read.csv(fn[id])
  
  if(summarize){
    print(summary(dat))
  } 
  
  return(dat)
}
