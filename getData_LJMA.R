# Written by Landon John Mueller-Amaral
# CS105 Programming Languages

# import your .csv file to global environment
data <- read.csv("cod.csv", header = TRUE, sep = ",")

# Given: a name -OR- code
# Return: Row of data referring to that username or code, and error if can't find it
get_data_by_name <- function(username){
  
  # import your .csv file to global environment
  data <- read.csv("cod.csv", header = TRUE, sep = ",")
  
  # This is how you get a specific column of data
  names <- data$name
  
  # --------------------------------------------- #
  
  # Search for the data
  index <- 1
  
  # iterating through the data
  for (n in names){
    
    # if there is a name there and it is the one we're looking for
    if(username == n){
      return(data[index, ])
    }
    
    # increase the reference index
    index <- index + 1
  }
  
  # you found nothing, return false
  return("There is no data by that username")
}


# Handles the strange form of COD usernames (Ex: username#usercode)
#
# returns an array
#   [1] - username
#   [2] - usercode
handle_name <- function(username){
  a <- strsplit(username, split = "#")
  return(c(a[[1]][1], a[[1]][2]))
}
