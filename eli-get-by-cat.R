
  # Construct the file path (modify as needed)
  # file_path <- file.path("/Users/school/github-classroom/cs-with-mike/2-1-2-Men-FInal-Project-", Cod_cwl_data.csv)
  #
  # if (!grepl("/", filename)) {
  #   filename <- file.path(getwd(), filename)
  # }
  #
  # if (!file.exists(filename)) {
  #   return(NULL)
  # }
  # set data to info from csv file
  csv_file <- "/Users/school/github-classroom/cs-with-mike/2-1-2-Men-FInal-Project-/Cod_cwl_data.csv"
  data <- read.csv(csv_file, header = TRUE)
  
  column_names <- colnames(data)

  # make matrix data using data

  matrix_data <- as.matrix(data)

  dimensions <- c(nrow(matrix_data), ncol(matrix_data))

  my_array <- array(matrix_data, dim = dimensions)
  colnames(my_array) <- column_names

get_by_cat <- function(array, category) {

  # find the index of the category in the column names of array

  category_index <- match(category, colnames(array))

  # check if the category is found
  if (is.na(category_index)) {
    cat("Category not found in the array.")
    return(NULL)
  }
  

  # extract the specified column and key rows, 1 and 9 are the column # and name of player, category_index is the specified category.
  result_array <- array[, c(9, category_index), drop = FALSE]
  result_array <- result_array[order(result_array[, 2], decreasing = TRUE), ]

  return(result_array)
}

  # Test code
  print(get_by_cat(my_array, "kills"))


