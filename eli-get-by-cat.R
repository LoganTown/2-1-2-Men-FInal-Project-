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
  if (!all(is.numeric(statArray[, 2]))) {
    cat("This stat is not numeric.\n")
    return(NULL)
  }

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
# getHighestStat(my_array, "map")

getStatList <- function(array){
  sortedColumns <- sort(colnames(array))
  cat("Stats: \n \n")
  cat(paste(sortedColumns, collapse = ", "), "\n")
}

getStatList(my_array)
