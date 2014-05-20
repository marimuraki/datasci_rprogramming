complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  files <- list.files(directory, full.names=TRUE) # creates a list of files
  mydata <- data.frame() # creates an empty data frame
  complete <- data.frame()
  for (i in id) { 
    # loops through the files counting number of complete cases
    mydata <- read.csv(files[i])
    nobs <- nrow(na.omit(mydata))
    complete <- rbind(complete, data.frame(id=i, nobs=nobs))
  }
  complete
}
