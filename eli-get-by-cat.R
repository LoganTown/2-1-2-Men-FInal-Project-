#ask for a list of csvs, for example: cod.csv, Cod_cwl_data.csv
csv_list <- readline("Enter the csv files you want to use, seperated by commas: ")
csv_list <- strsplit(csv_list, ",")[[1]]
csv_list <- trimws(csv_list)


# Function: makeArray
# Input:
#   - csv_name: A character string representing the file path or name of a CSV file.
#              The CSV file is assumed to have headers, and its data will be used to create an array.
# Output:
#   - myArray: A 2D array (matrix) representing the data from the CSV file.
#               The column names of the array are set to the column names from the CSV file.
#               If the CSV file has 'm' rows and 'n' columns, the array dimensions will be c(m, n).
#               The function returns this array as its output.
makeArray <- function(csv_name) {
  data <- read.csv(csv_name)
  columnNames <- colnames(data)
  matrixData <- as.matrix(data)
  dimensions <- c(nrow(matrixData), ncol(matrixData))
  myArray <- array(matrixData, dim = dimensions)
  colnames(myArray) <- columnNames
  return(myArray)
}

data_array <- lapply(csv_list, makeArray) # access with double brackets








# Function: getByCat
# Input:
#   - array: The 2D array containing player data.
#   - category: The category (stat) to retrieve from the array.
# Output:
#   - resultArray: A 2D array containing player names and the specified category,
#      
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


# Function: getHighestStat
# Input:
#   - array: The 2D array containing player data.
#   - stat: The specific stat to find the highest value for.
# Output:
#   - Prints information about the player(s) with the highest specified stat.
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


# Function: getStatList
# Input:
#   - array: The 2D array containing player data.
#   - page: The page number or "all" to view all stats.
# Output:
#   - Prints a list of stats either for the specified page or all stats.
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


# Function: getStatListRunner
# Input:
#   - array_list: A list of 2D arrays containing player data.
# Output:
#   - Prints stat lists and interacts with the user to navigate through the stats.
getStatListRunner <- function(array_list) {

  page <- 1

  while (TRUE) {
    # Iterate through each array in the list
    for (i in seq_along(array_list)) {
      getStatList(array_list[[i]], page)
    }

    userInput <- readline("Enter '<' to move backward, '>' to move forward, 'a' to view all, or 'x' to exit: ")

    if (userInput == "x") {
      break
    } else if (userInput == "<") {
      page <- max(1, page - 1)
    } else if (userInput == ">") {
      page <- page + 1
    } else if (userInput == "a") {
      # Print all stats for each array in the list
      for (i in seq_along(array_list)) {
        getStatList(array_list[[i]], "all")
      }
      break
    } else {
      cat("Invalid input. Please enter '<', '>', 'a', or 'x'.\n")
    }
  }
}


# Function: getPlayerList
# Input:
#   - array: The 2D array containing player data.
#   - page: The page number or "all" to view all players.
# Output:
#   - Prints a list of players either for the specified page or all players.
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


# Function: getPlayerListRunner
# Input:
#   - array_list: A list of 2D arrays containing player data.
# Output:
#   - Prints player lists and interacts with the user to navigate through the players.
getPlayerListRunner <- function(array_list) {
  page <- 1

  while (TRUE) {
    # Iterate through each array in the list
    for (i in seq_along(array_list)) {
      getPlayerList(array_list[[i]], page)
    }

    # Prompt user for input
    userInput <- readline("Enter '<' to move backward, '>' to move forward, 'a' to view all, or 'x' to exit: ")

    if (userInput == "x") {
      break
    } else if (userInput == "<") {
      page <- max(1, page - 1)
    } else if (userInput == ">") {
      page <- page + 1
    } else if (userInput == "a") {
      # Print all players for each array in the list
      for (i in seq_along(array_list)) {
        getPlayerList(array_list[[i]], "all")
      }
      break
    } else {
      cat("Invalid input. Please enter '<', '>', 'a', or 'x'.\n")
    }
  }
}


# Function: compareSpecificStats
# Input:
#   - array: The 2D array containing player data.
#   - players: Vector of player names to compare.
#   - categories: Vector of stat categories to compare.
# Output:
#   - Prints a 2D array containing the specified players and categories.
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
