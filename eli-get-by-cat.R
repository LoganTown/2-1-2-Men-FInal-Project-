source("eli-graph-stats.R")

#ask for a list of csvs, for example: cod.csv, Cod_cwl_data.csv

csv_list <- readline("Enter the csv files you want to use, seperated by commas: ")
#csv_list <- c("cod.csv", "Cod_cwl_data.csv") #temp, remove later but makes it so i dont have to enter csv each time
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
#   - array_list: A list of 2D arrays containing player data.
#   - stat: The specific stat to find the highest value for.
# Output:
#   - Prints information about the player(s) with the highest specified stat.
getHighestStat <- function(array_list, stat) {
  maxValue <- -Inf #R THING? 
  overallMaxPlayers <- data.frame(stringsAsFactors = FALSE)
  
  for (array in array_list) {
    #check if stat is present in array
    
    if (stat %in% colnames(array)) {
      #find relevant columns for current array
      relevantCols <- c(which(tolower(colnames(array)) == tolower(stat)), 
                        which(tolower(colnames(array)) %in% c("name", "player")))
      
      statArray <- array[, relevantCols, drop = FALSE]
      
      # If stat isn't numeric, it isn't comparable
      numeric_values <- as.numeric(trimws(statArray[, 1]))
      
      if (any(is.na(numeric_values) | !is.finite(numeric_values))) {
        cat("This stat is not numeric for one of the arrays.\n")
        returnValue(NULL)
      } else {
        # find the max value in the specified stat column
        bigValue <- max(as.numeric(statArray[, 1]))
        
        #check if bigger than current stat, replace if it is
        if (maxValue < bigValue) {
          maxValue <- bigValue
          
          # Rows that have the overall max value as their stat value are saved as a variable
          maxRows <- which(as.numeric(statArray[, 1]) == maxValue)
          
          #save players in current biggest row
          overallMaxPlayers <- statArray[maxRows, ]
        }


      }
      
    } else {
      cat("Specified stat not found in one of the arrays.\n")
    }
  }
  
  # Print the overall highest stat, value, and player information
  cat("The highest ", stat, " is", maxValue, ". \n")
  cat("Players:\n")
  print(overallMaxPlayers)
  plotStatHistogram(array_list, stat)


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
  # Find the column index containing player names
  playerColumnIndex <- which(tolower(colnames(array)) %in% c("player", "name"))
  
  if (length(playerColumnIndex) == 0) {
    cat("Error: Player names column not found in the array.\n")
    return(NULL)
  }
  
  playerList <- unique(array[, playerColumnIndex])
  sortedPlayerList <- sort(playerList)

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
#   - array_list: A list of 2D arrays containing player data.
#   - players: Vector of player names to compare.
#   - categories: Vector of stat categories to compare.
# Output:
#   - Prints a 2D array containing the specified players and categories.
compareSpecificStats <- function(array_list, players, categories) {
  for (array in array_list) {
    playerIndices <- which(colnames(array) %in% c("player", "name"))
    categoryIndices <- match(tolower(categories), tolower(colnames(array)))

    if (any(is.na(playerIndices)) || any(is.na(categoryIndices))) {
      cat("One or more players or categories not found in the array.\n")
    } else {
      # Identify the first row corresponding to player1 and player2
      player1Index <- which(array[, playerIndices] %in% players[1])[1]
      player2Index <- which(array[, playerIndices] %in% players[2])[1]
      
      # Extract data for player1 and player2
      dataPlayer1 <- array[player1Index, categoryIndices, drop = FALSE]
      dataPlayer2 <- array[player2Index, categoryIndices, drop = FALSE]
      
      # Label columns with corresponding categories
      colnames(dataPlayer1) <- colnames(array)[categoryIndices]
      colnames(dataPlayer2) <- colnames(array)[categoryIndices]

      # Print arrays with player names
      cat("Data for", players[1], ":\n")
      print(dataPlayer1)
      
      cat("Data for", players[2], ":\n")
      print(dataPlayer2)

      print(createPlayerBarGraph(dataPlayer1, players[1]))
      print(createPlayerBarGraph(dataPlayer2, players[2]))

    }
  }
}


# comparePlayers function: Compares statistics between two players for selected categories.
# Inputs:
#   - array_list: A list of 2D arrays containing player data.
#   - player1: The name of the first player for comparison.
#   - player2: The name of the second player for comparison.
# Output:
#   - Comparison of statistics between the two players for selected categories.
comparePlayers <- function(array_list, player1, player2){
  players <- c(player1, player2)
  categories <- c("kills" , "assists" , "deaths" , "scorestreaks.kills" , "accuracy...." , "shots" , "time.alive..s." , "avg.time.per.life..s.")
  compareSpecificStats(array_list, players, categories)
}


# getPlayerProfile function: Extracts and displays a player's statistics profile.
# Inputs:
#   - array: A matrix or data frame containing player statistics.
#   - player: The name of the player whose profile is to be retrieved.
# Output:
#   - A matrix containing the player's statistics for selected categories.
getPlayerProfile <- function(array_list, player) {
  playerProfiles <- c()
  
  categories <- c("kills" , "assists" , "deaths" , "scorestreaks.kills" , "accuracy...." , "shots" , "time.alive..s." , "avg.time.per.life..s.")
  
  for (array in array_list) {
    # Find the player indices in all columns
    playerIndices <- which(array == player, arr.ind = TRUE)
    
    # Find category indices in all columns
    categoryIndices <- which(tolower(colnames(array)) %in% tolower(categories))
    
    if (nrow(playerIndices) > 0) {
      # Extract player's statistics for the specified categories
      resultArray <- array[playerIndices[, "row"], categoryIndices, drop = FALSE]
      
      # Transpose the resultArray to have categories as rows
      resultArray <- t(resultArray)
      
      # Save the result in the playerProfiles list
      playerProfiles[[length(playerProfiles) + 1]] <- resultArray
    }
  }
  
  if (length(playerProfiles) == 0) {
    cat("Player not found in any array.\n")
    return(NULL)
  }
  
  cat(paste0(player, "'s stat profiles:\n"))
  for (i in seq_along(playerProfiles)) {
    cat("Array ", i, ":\n")
    print(playerProfiles[[i]])
  }
}



