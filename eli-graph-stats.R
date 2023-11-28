library(ggplot2)
library(dplyr)

# Sample esports data - testing sample values to figure out how graph works
esports_data <- data.frame(
  Player = c("Player1", "Player2", "Player3", "Player4"),
  Score = c(120, 90, 110, 80),
  Kills = c(50, 40, 60, 30),
  Deaths = c(20, 15, 25, 18),
  Assists = c(30, 25, 35, 20)
)

# Function to compare stats between two players and display bar graphs
comparePlayersWithBarGraphs <- function(player_data, player1, player2) {
  player1_row <- player_data[player_data$Player == player1, ]
  player2_row <- player_data[player_data$Player == player2, ]

  if (nrow(player1_row) == 0 || nrow(player2_row) == 0) {
    cat("One or both players not found. Please check the player names and try again.\n")
    return(NULL)
  }

  # Comparison of specific stats for selected players
  selected_players <- player_data[player_data$Player %in% c(player1, player2), ]
  comparison_data_long <- tidyr::pivot_longer(selected_players, cols = c("Kills", "Deaths", "Assists"), names_to = "Stat", values_to = "Value")

  # Visualization: Bar chart of selected stats for each player
  ggplot(comparison_data_long, aes(x = Player, y = Value, fill = Stat)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7) +
    labs(title = paste("Comparison of Specific Stats between", player1, "and", player2),
         x = "Player",
         y = "Value",
         fill = "Stat") +
    theme_minimal()
}

# Example usage
player1 <- "Player1"
player2 <- "Player2"
comparePlayersWithBarGraphs(esports_data, player1, player2)
