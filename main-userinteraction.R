cat("Welcome to the Logan, Landon, and Eli's Call of Duty Data Analyzer!\n")

# Imports functions from other programs
source("compile-info-functions.R")

esportsData <- data_array

while (TRUE) {
  cat("\nChoose an option:\n")
  cat("1. Get player profile\n")
  cat("2. Compare stats between two players\n")
  cat("3. Compare specific stats of multiple players\n")
  cat("4. Get the player(s) with the highest specified stat\n")
  cat("5. Exit\n")
  
  choice <- as.numeric(readline("Enter your choice: "))
  
# Choice 1 gets the full stats of an individual player; their profile
  if (choice == 1) {      
    playerName <- readline("Enter the player name (enter 'PL' for player list): ")
    if (playerName != 'PL') {
      getPlayerProfile(esportsData, playerName)
    } else {
      getPlayerListRunner(esportsData)
    }
    
# Choice 2 compares the full stats of two different players
  } else if (choice == 2) {      
    repeat {
      player1 <- readline("Enter the first player name (enter 'PL' for player list): ")
      if (player1 == 'PL') {
        getPlayerListRunner(esportsData)
        break
      } else {
        player2 <- readline("Enter the second player name (enter 'PL' for player list): ")
        if (player2 == 'PL') {
          getPlayerListRunner(esportsData)
          break
        } else {
          comparePlayers(esportsData, player1, player2)
          break
        }
      }
    }
    
# Choice 3 compares specific stats of multiple different players
  } else if (choice == 3) {      
    repeat {
      playerNames <- strsplit(readline("Enter player names (case-sensitive; comma-separated; enter 'PL' for player list): "), ",")[[1]]
      playerNames <- trimws(playerNames)
      if (playerNames[1] == 'PL') {
        getPlayerListRunner(esportsData)
      } else {
        statsToCompare <- strsplit(readline("Enter stats to compare (comma-separated; enter 'SL' for stat list): "), ",")[[1]]
        statsToCompare <- trimws(statsToCompare)
        if (statsToCompare[1] != 'SL') {
          compareSpecificStats(esportsData, playerNames, statsToCompare)
          break
        } else {
          getStatListRunner(esportsData)
        }
      }
    }

# Choice 4 gets player(s) with the highest specified stat    
  } else if (choice == 4) {  
    stat <- readline("Enter stat (enter 'SL' for stat list): ")
    if (stat != 'SL') {
      getHighestStat(esportsData, stat)
    } else {
      getStatListRunner(esportsData)
    }
    
# choice 5 exit, if invalid then ask for a valid option
  } else if (choice == 5) {
    cat("Exiting program.\n")
    break
  } else {
    cat("Invalid choice. Please enter a valid option.\n")
  }
}