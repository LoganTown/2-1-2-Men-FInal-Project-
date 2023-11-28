# Eli's User Interaction Program (Main.R)

# Possible functions to write:

# - getPlayerProfile(esports_data, player_name)
#   Parameters: data, player name
#   Returns: print statement of player name and stats

# - comparePlayers(esports_data, player1, player2)
#   Parameters: data, two player names
#   Returns: print statement of two players getPlayerProfile, maybe side by side for easy comparison?

# - getPlayerList(esports_data)
#   Parameters: data
#   Returns: print statement of all player names, sorted alphabetically maybe?

# - getStatList(esports_data)
#   Parameters: data
#   Returns: print statement of all stats, sorted alphabetically maybe?

# - compareSpecificStats(esports_data, player_names, stats_to_compare)
#   Parameters: data, list of player names, list of stats to compare
#   Returns: print statement of multiple players' specific stats

# NOT IMPLEMENTED (ideas):

# - getHighestStat(esports_data, stat)
#   Parameters: data, stat
#   Returns: print statement with player name and their specific specified stat, error if not numerical value

# - getTopStat(esports_data, player_count, stat)
#   Parameters: data, number of players to display, stat to display
#   Returns: print statement with top player_count players and their stat, error if not numerical value

# Imports functions from other programs

source("eli-get-by-cat.R")
source("logan-get-by-filter.r")
source("getData_LJMA.R")
# source("any more programs we write.R")

cat("Welcome to the Logan, Landon, and Eli's Call of Duty Data Analyzer!\n")

# While loop that allows users to do different things with the data
esportsData <- myArray

while (TRUE) {
  cat("\nChoose an option:\n")
  cat("1. Get player profile\n")
  cat("2. Compare stats between two players\n")
  cat("3. Compare specific stats of multiple players\n")
  cat("4. Get the player(s) with the highest specified stat\n")
  cat("5. Exit\n")

  # Stores the number choice as a variable
  choice <- as.numeric(readline("Enter your choice: "))

  # Choice 1 gets the full stats of an individual player; their profile
  if (choice == 1) {
    playerName <- readline("Enter the player name: ")
    getPlayerProfile(esportsData, playerName)

  # Choice 2 compares the full stats of two different players
  } else if (choice == 2) {
    repeat {
      player1 <- readline("Enter the first player name (enter 'PL' for player list): ")
      if (player1 != 'PL') {
        player2 <- readline("Enter the second player name (enter 'PL' for player list): ")
      }
      if (player1 != 'PL' & player2 != 'PL') {
        comparePlayers(esportsData, player1, player2)
        break
      } else {
        getPlayerListRunner()
      }
    }

  # Choice 3 compares specific stats of multiple different players
  } else if (choice == 3) {
    repeat{
    playerNames <- strsplit(readline("Enter player names (comma-separated; enter 'PL' for player list): "), ",")[[1]]
    if (playerNames == 'PL') {
      getPlayerListRunner()
    } else {
      statsToCompare <- strsplit(readline("Enter stats to compare (comma-separated; enter 'SL' for stat list): "), ",")[[1]]

    if (statsToCompare != 'SL') {
      compareSpecificStats(esportsData, playerNames, statsToCompare)
      break
    } else {
      getStatListRunner()
    }}}

  # Choice 4 gets player(s) with highest specified stat
  } else if (choice == 4) {
    stat <- strsplit(readline("Enter stat (enter 'SL' for stat list): "), ",")[[1]]
    if (stat != 'SL') {
      getHighestStat(myArray, stat)
    } else {
      getStatListRunner()
    }

  # Choice 5 exits
  } else if (choice == 5) {
    cat("Exiting program.\n")
    break

  # If not one of the options, try again
  } else {
    cat("Invalid choice. Please enter a valid option.\n")
  }
}
