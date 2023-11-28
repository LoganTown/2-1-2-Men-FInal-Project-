# Construct the file path (modify as needed)
# filePath <- file.path("/Users/school/github-classroom/cs-with-mike/2-1-2-Men-FInal-Project-", CodCwlData.csv)
#
# if (!grepl("/", filename)) {
#   filename <- file.path(getwd(), filename)
# }
#
# if (!file.exists(filename)) {
#   return(NULL)
# }
# Set data to info from CSV file
csvFile <- "/Users/school/github-classroom/cs-with-mike/2-1-2-Men-FInal-Project-/Cod_cwl_data.csv"
data <- read.csv(csvFile, header = TRUE)

columnNames <- colnames(data)

# Make matrix data using data
matrixData <- as.matrix(data)

dimensions <- c(nrow(matrixData), ncol(matrixData))

myArray <- array(matrixData, dim = dimensions)
colnames(myArray) <- columnNames

getByCat <- function(array, category) {

  # Find the index of the category in the column names of array
  categoryIndex <- match(category, colnames(array))

  # Check if the category is found
  if (is.na(categoryIndex)) {
    cat("Category not found in the array.")
    return(NULL)
  }

  # Extract the specified column and key rows
  resultArray <- array[, c(9, categoryIndex), drop = FALSE]
  resultArray <- resultArray[order(resultArray[, 2], decreasing = TRUE), ]

  return(resultArray)
}

# Test code
# print(getByCat(myArray, "kills"))

getHighestStat <- function(array, stat){

  # Sets the array to be used as an array with just the necessary stat
  statArray <- getByCat(array, stat)

  # If stat isn't numeric, it isn't comparable
if (!all(sapply(trimws(statArray[, 2]), function(x) grepl("^\\s*-?\\d*\\.?\\d+\\s*$", x)))) {
  cat("This stat is not numeric.\n")
  return(NULL)}

  # Saves the max value of the desired stat as a variable
  maxValue <- max(statArray[, 2])

  # Rows that have the max value as their stat value are saved as a variable
  maxRows <- which(statArray[, 2] == maxValue)

  # Players in these rows are saved as a variable
  maxPlayers <- statArray[maxRows, ]

  # Prints everything nicely!
  cat("The highest ", stat, " is", maxValue, ". \n")
  cat("Players:\n")
  print(maxPlayers)
}

# Test code
# getHighestStat(myArray, "map")

getStatList <- function(array){
  sortedColumns <- sort(colnames(array))
  cat("Stats: \n \n")
  cat(paste(sortedColumns, collapse = ", "), "\n")
}

# Test code
# getStatList(my_array)

getPlayerList <- function(array, page) {
  # Extract the column with player names
  playerList <- array[, 9]

  # Remove duplicates from the player list
  uniquePlayerList <- unique(playerList)

  # Sort the unique player list alphabetically
  sortedPlayerList <- sort(uniquePlayerList)

  if (page == "all") {
    # Display all players at once
    cat("All Players (Alphabetical Order, No Duplicates):\n")
    cat(sortedPlayerList, sep = "\n")
  } else {
    # Calculate the starting index for the current page
    start_index <- (page - 1) * 10 + 1

    # Calculate the ending index for the current page
    end_index <- min(page * 10, length(sortedPlayerList))

    # Extract the players for the current page
    current_page_players <- sortedPlayerList[start_index:end_index]

    # Print the current page of players
    cat("Page", page, "of Players (Alphabetical Order, No Duplicates):\n")
    cat(current_page_players, sep = "\n")
  }
}


getPlayerListRunner <- function() {

  page <- 1

  while (TRUE) {
    getPlayerList(myArray, page)

    # Prompt user for input
    userInput <- readline("Enter '<' to move backward, '>' to move forward, 'a' to view all, or 'x' to exit: ")

    if (userInput == "x") {
      cat("Exiting the loop.\n")
      break
    } else if (userInput == "<") {
      # Move backward if possible
      page <- max(1, page - 1)
    } else if (userInput == ">") {
      # Move forward if possible
      page <- page + 1
    } else if (userInput == "a") {
      # View all players at once
      getPlayerList(myArray, "all")
      break  # exit the loop after viewing all players
    } else {
      cat("Invalid input. Please enter '<', '>', 'a', or 'x'.\n")
    }
  }
}
