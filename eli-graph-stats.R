library(ggplot2)
library(dplyr)

plotStatHistogram <- function(array_list, stat) {
  for (array in array_list) {
    if (stat %in% colnames(array)) {
      relevantCols <- c(which(tolower(colnames(array)) == tolower(stat)),
                          which(tolower(colnames(array)) %in% c("name", "player")))

      statArray <- array[, relevantCols, drop = FALSE]

      numeric_values <- as.numeric(trimws(statArray[, 1]))

      if (any(is.na(numeric_values) | !is.finite(numeric_values))) {
        cat("This stat is not numeric for one of the arrays.\n")
      } else {
        # Plot the histogram for the current stat
        hist(numeric_values, main = paste("Histogram of", stat),
             xlab = stat, ylab = "Frequency", col = "lightblue", border = "black")
      }
    } else {
      cat("Specified stat not found in one of the arrays.\n")
    }
  }
}

plotComparisonBarGraph <- function(player1Data, player2Data, categories, playerNames) {
  par(mfrow = c(1, 2))  # Set up a 1x2 grid for side-by-side plots

  # Plot bar graphs for player1 and player2 side by side
  barplot(as.matrix(player1Data), beside = TRUE, col = rainbow(length(categories)), main = playerNames[1])
  barplot(as.matrix(player2Data), beside = TRUE, col = rainbow(length(categories)), main = playerNames[2])

  par(mfrow = c(1, 1))  # Reset to a single plot
}

createPlayerBarGraph <- function(player_data, player_name) {
  player_data_df <- as.data.frame(player_data)
  player_data_df[] <- lapply(player_data_df, as.numeric)
  player_data_long <- tidyr::gather(player_data_df, key = "variable", value = "value")

  ggplot(player_data_long, aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Player:", player_name),
         x = "Category",
         y = "Value",
         fill = "Category") +
    theme_minimal()
}


