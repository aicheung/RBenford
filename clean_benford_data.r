#' Cleans a single-column CSV file for Benford's Law analysis.
#'
#' This function reads a single-column CSV file, cleans the data by
#' removing blank, empty, zero, and leading-zero values, and converts
#' string representations of numbers to numeric.
#'
#' @param file_path A string specifying the path to the CSV file.
#' @return A numeric vector containing the cleaned data, or an error if
#'         the file cannot be read or no valid numeric data remains.
#' @examples
#' # Create a dummy CSV file for demonstration
#' cat("TransactionAmount\n",
#'     "123.45\n",
#'     "0.99\n",
#'     "0123\n", # Should be removed (starts with zero)
#'     "456\n",
#'     "\n",     # Blank row
#'     "   \n",  # Empty row with spaces
#'     "0\n",    # Zero value
#'     "789.00\n",
#'     "abc\n",  # Non-numeric string (will be coerced to NA and removed)
#'     "  543.21  \n", # String with leading/trailing spaces
#'     file = "dummy_data.csv")
#'
#' cleaned_data <- clean_benford_data("dummy_data.csv")
#' print(cleaned_data)
#'
#' # Clean up the dummy file
#' unlink("dummy_data.csv")
clean_benford_data <- function(file_path) {
  # 1. Read the file
  if (!file.exists(file_path)) {
    stop("Error: File not found at specified path: ", file_path)
  }
  
  # Read the CSV as a data frame. Using header = TRUE and as.is = TRUE
  # to prevent R from converting strings to factors prematurely.
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Check if there's exactly one column
  if (ncol(data) != 1) {
    stop("Error: The CSV file must contain exactly one column.")
  }
  
  # Get the column name (for informative messages)
  col_name <- names(data)[1]
  
  # Extract the single column as a vector
  values <- data[[col_name]]
  
  # 2. Remove all blank, empty, zero values or numbers that start with zero
  
  # Convert to character to handle mixed types (numeric, string, NA)
  values_char <- as.character(values)
  
  # Remove leading/trailing whitespace
  values_char <- trimws(values_char)
  
  # Identify and remove truly empty strings or those only containing whitespace
  values_char <- values_char[values_char != "" & values_char != " "]
  
  # Identify and remove values that are "0"
  values_char <- values_char[values_char != "0"]
  
  # Identify and remove numbers that start with zero (e.g., "0123", "05")
  # This regex matches strings that start with '0' followed by any digit or a dot.
  values_char <- values_char[!grepl("^0[0-9]", values_char)]
  
  # 3. If the cell is string value (e.g. "123") convert it as number
  # Attempt to convert to numeric. Non-numeric values will become NA.
  cleaned_numeric <- as.numeric(values_char)
  
  # Remove NA values resulting from non-numeric strings
  cleaned_numeric <- cleaned_numeric[!is.na(cleaned_numeric)]
  
  # Ensure all remaining values are positive (Benford's Law typically applies to magnitudes)
  # This also implicitly removes any negative numbers if they made it this far,
  # though the problem statement didn't explicitly ask for this, it's good practice for Benford.
  cleaned_numeric <- cleaned_numeric[cleaned_numeric > 0]
  
  if (length(cleaned_numeric) == 0) {
    warning("No valid numeric data remaining after cleaning.")
    return(numeric(0)) # Return an empty numeric vector
  }
  
  return(cleaned_numeric)
}