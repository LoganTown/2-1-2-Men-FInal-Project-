# A function that filters rows from a CSV file based on a specified amount in a given column.
# Arguments:
#   - filename: The name of the CSV file.
#   - amount: The threshold amount for filtering rows.
#   - columnName: The name of the column used for filtering.
# Returns:
#   A list of rows from the CSV file that satisfy the filter condition.

get_by_filter_greater <- function(filename, amount, columnName) {
  
  # Construct the file path (modify as needed)
  #file_path <- file.path("C:/Users/ltown/Desktop/temp final project", filename)
  
  if (!grepl("/", filename)) {
    filename <- file.path(getwd(), filename)
  }
  
  if (!file.exists(filename)) {
    return(NULL)
  }
  
  # Read the CSV file and store it in the 'data' variable
  data <- read.csv(filename)
  
  if (!(columnName %in% names(data))) {
    return(NULL)
  }
  
  # Extract the specified column for filteringa
  filtered_rows <- data[, columnName]
  
  # Initialize an empty list to store rows that meet the filter condition
  storage <- list()
  
  # Iterate through each row in the data
  for (i in 1:nrow(data)) {
    # Check if the value in the specified column is greater than the threshold amount
    if (filtered_rows[i] > amount) {
      # Append the current row to the 'storage' list
      storage[[length(storage) + 1]] <- data[i, ]
    }
  }
  
  # Return the list of rows that satisfy the filter condition
  return(storage)
}

# A function that filters rows from a CSV file based on a specified amount in a given column.
# Arguments:
#   - filename: The name of the CSV file.
#   - amount: The max amount for filtering rows.
#   - columnName: The name of the column used for filtering.
# Returns:
#   A list of rows from the CSV file that satisfy the filter condition.

get_by_filter_lesser <- function(filename, amount, columnName) {
  
  # Construct the file path (modify as needed)
  #file_path <- file.path("C:/Users/ltown/Desktop/temp final project", filename)
  
  if (!grepl("/", filename)) {
    filename <- file.path(getwd(), filename)
  }
  
  if (!file.exists(filename)) {
    return(NULL)
  }
  
  # Read the CSV file and store it in the 'data' variable
  data <- read.csv(filename)
  
  if (!(columnName %in% names(data))) {
    return(NULL)
  }
  
  # Extract the specified column for filteringa
  filtered_rows <- data[, columnName]
  
  # Initialize an empty list to store rows that meet the filter condition
  storage <- list()
  
  # Iterate through each row in the data
  for (i in 1:nrow(data)) {
    # Check if the value in the specified column is greater than the threshold amount
    if (filtered_rows[i] < amount) {
      # Append the current row to the 'storage' list
      storage[[length(storage) + 1]] <- data[i, ]
    }
  }
  
  # Return the list of rows that satisfy the filter condition
  return(storage)
}