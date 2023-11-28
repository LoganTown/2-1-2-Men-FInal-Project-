# Eli's User Interaction Program (Main.R)

# possible functions to write:

# - getPlayerProfile(esports_data, player_name)
#     parameters: data, player name
#     returns: print statement of player name and stats

# - comparePlayers(esports_data, player1, player2)
#     parameters: data, two player names
#     returns: print statement of two players getPlayerProfile, maybe side by side for easy comparison?

# - getPlayerList(esports_data)
#     parameters: data
#     returns: print statement of all player names, sorted alphabetically maybe?

# - getStatList(esports_data)
#     parameters: data
#     returns: print statement of all stats, sorted alphabetically maybe?

# - compareSpecificStats(esports_data, player_names, stats_to_compare)
#     parameters: data, list of player names, list of stats to compare
#     returns: print statement of multiple players specific stats

# NOT IMPLEMENTED (ideas):

# - getHighestStat(esports_data, stat)
#     parameters: data, stat
#     returns: print statement with player name and their specific specified stat, error if not numerical value

# - getTopStat(esports_data, player_count, stat)
#     parameters: data, number of players to display, stat to display
#     returns: print statement with top player_count players and their stat, error if not numerical value

# imports functions from other programs

source("eli-get-by-cat.R")
source("logan-get-by-filter.r")
source("getData_LJMA.R")
# source("any more programs we write.R")

cat("Welcome to the Logan, Landon, and Eli's Call of Duty Data Analyzer!\n")

# while loop that allows users to do different things with the data
esports_data <- my_array

while (TRUE) {
  cat("\nChoose an option:\n")
  cat("1. Get player profile\n")
  cat("2. Compare stats between two players\n")
  cat("3. Compare specific stats of multiple players\n")
  cat("4. Get the player(s) with the highest specified stat\n")
  cat("5. Exit\n")

  # stores the number choice as a variable

  choice <- as.numeric(readline("Enter your choice: "))

  # choice 1 gets the full stats of an individual player; their profile

  if (choice == 1) {
    player_name <- readline("Enter the player name: ")
    getPlayerProfile(esports_data, player_name)

  # choice 2 compares the full stats of two different players

  } else if (choice == 2) {
    repeat {
      player1 <- readline("Enter the first player name (enter 'PL' for player list): ")
        if (player1 != 'PL'){
          player2 <- readline("Enter the second player name (enter 'PL' for player list): ")
        }
    if (player1 != 'PL' & player2 != 'PL') {
      comparePlayers(esports_data, player1, player2)
      break
    }
    else {
      getPlayerList(esports_data)
    }}

  # choice 3 compares specific stats of multiple different players

  } else if (choice == 3) {
    player_names <- strsplit(readline("Enter player names (comma-separated; enter 'PL' for player list): "), ",")[[1]]
    if (player_names == 'PL'){
      getPlayerList(esports_data)
    }
    else {
      stats_to_compare <- strsplit(readline("Enter stats to compare (comma-separated; enter 'SL' for stat list): "), ",")[[1]]
    }
    if (stats_to_compare != 'SL'){
      compareSpecificStats(esports_data, player_names, stats_to_compare)
    }
    else {
      getStatList(esports_data)
    }}
    # choice 4 gets player(s) with highest specified stat
    else if (choice == 4){
    stat <- strsplit(readline("Enter stat (enter 'SL' for stat list): "), ",")[[1]]
    if (stat != 'SL'){
      getHighestStat(my_array, stat)
    }
    else{
      getStatList(my_array)
    }

  }
  # choice 5 exits

   else if (choice == 5) {
    cat("Exiting program.\n")
    break

  # if not one of the options, try again

  } else {
    cat("Invalid choice. Please enter a valid option.\n")
  }
}