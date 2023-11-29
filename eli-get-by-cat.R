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
  categoryIndex <- match(tolower(category), colnames(array))

  # Check if the category is found
  if (is.na(categoryIndex)) {
    cat("Stat not found in the data.")
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
  statArray <- getByCat(array, tolower(stat))

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

getStatList <- function(array, page) {
  sortedColumns <- sort(colnames(array))

  if (page == "all") {
    cat("All Stats:\n\n")
    cat(paste(sortedColumns, collapse = "\n"), "\n")
  } else {
    startIndex <- (page - 1) * 10 + 1
    endIndex <- min(page * 10, length(sortedColumns))
    currentPageStats <- sortedColumns[startIndex:endIndex]

    cat("Page", page, "of Stats:\n")
    cat(paste(currentPageStats, collapse = "\n"), "\n")
  }
}


getStatListRunner <- function() {

  page <- 1

  while (TRUE) {
    getStatList(myArray, page)

    userInput <- readline("Enter '<' to move backward, '>' to move forward, 'a' to view all, or 'x' to exit: ")

    if (userInput == "x") {
      break
    } else if (userInput == "<") {
      page <- max(1, page - 1)
    } else if (userInput == ">") {
      page <- page + 1
    } else if (userInput == "a") {
      getStatList(myArray, "all")
      break
    } else {
      cat("Invalid input. Please enter '<', '>', 'a', or 'x'.\n")
    }
  }
}

# Test code
# getStatList(my_array)

getPlayerList <- function(array, page) {
  playerList <- array[, 9]
  uniquePlayerList <- unique(playerList)
  sortedPlayerList <- sort(uniquePlayerList)

  if (page == "all") {
    cat("All Players (Alphabetical Order, No Duplicates):\n")
    cat(sortedPlayerList, sep = "\n")
  } else {
    # Calculate the starting index for the current page
    startIndex <- (page - 1) * 10 + 1

    # Calculate the ending index for the current page
    endIndex <- min(page * 10, length(sortedPlayerList))

    currentPagePlayers <- sortedPlayerList[startIndex:endIndex]

    # Print the current page of players
    cat("Page", page, "of Players (Alphabetical Order, No Duplicates):\n")
    cat(currentPagePlayers, sep = "\n")
  }
}

getPlayerListRunner <- function() {

  page <- 1

  while (TRUE) {
    getPlayerList(myArray, page)

    # Prompt user for input
    userInput <- readline("Enter '<' to move backward, '>' to move forward, 'a' to view all, or 'x' to exit: ")

    if (userInput == "x") {
      break
    } else if (userInput == "<") {
      page <- max(1, page - 1)
    } else if (userInput == ">") {
      page <- page + 1
    } else if (userInput == "a") {
      getPlayerList(myArray, "all")
      break
    } else {
      cat("Invalid input. Please enter '<', '>', 'a', or 'x'.\n")
    }
  }
}

compareSpecificStats <- function(array, players, categories) {
  playerIndices <- match(players, array[, 9])

  categoryIndices <- match(tolower(categories), colnames(array))

  if (any(is.na(playerIndices)) || any(is.na(categoryIndices))) {
    cat("One or more players or categories not found in the array.\n")
    return(NULL)
  }

  resultArray <- array[playerIndices, c(9, categoryIndices), drop = FALSE]

  print(resultArray)
}
