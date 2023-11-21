
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
  
  print(column_names)
  # make matrix data using data

  matrix_data <- as.matrix(data)

  dimensions <- c(nrow(matrix_data), ncol(matrix_data))

  my_array <- array(matrix_data, dim = dimensions)
  colnames(my_array) <- column_names
  View(my_array)

get_by_cat <- function(array, column_names, category) {

  # find the index of the category in the column names of array

  category_index <- match(category, colnames(array))

  # check if the category is found
  if (is.na(category_index)) {
    cat("Category not found in the array.")
    return(NULL)
  }
  

  # extract the specified column and key rows
  result_array <- array[, c(category_index, 1), drop = FALSE]


  return(result_array)
}

