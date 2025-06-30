#' Extracts leading digits from a numeric vector for Benford's Law analysis.
#'
#' This function takes a numeric vector and extracts the first digit, the second digit,
#' or both, based on the specified mode. It's designed for use with cleaned data
#' where values are positive and non-zero.
#'
#' @param numbers A numeric vector of positive, non-zero values.
#' @param mode An integer specifying which digits to extract:
#'   1: Extracts the first leading digit.
#'   2: Extracts the second leading digit.
#'   3: Extracts both the first and second leading digits (returns a list).
#' @return A numeric vector (for mode 1 or 2) or a list of two numeric vectors
#'         (for mode 3). Returns an empty vector or an empty list if input is empty
#'         or no valid digits can be extracted.
#' @examples
#' # Example data (assuming it's already cleaned)
#' data_for_digits <- c(123, 45, 6789, 1, 9.87, 20.5, 3000)
#'
#' # Mode 1: First digit only
#' first_digits <- extract_leading_digits(data_for_digits, mode = 1)
#' print("First digits:")
#' print(first_digits)
#'
#' # Mode 2: Second digit only
#' second_digits <- extract_leading_digits(data_for_digits, mode = 2)
#' print("Second digits:")
#' print(second_digits)
#'
#' # Mode 3: First and second digits
#' all_digits <- extract_leading_digits(data_for_digits, mode = 3)
#' print("First and second digits (list):")
#' print(all_digits$first_digits)
#' print(all_digits$second_digits)
#'
#' # Example with numbers less than 1 (but > 0)
#' data_small <- c(0.123, 0.045, 0.6789, 0.001, 0.987, 0.205)
#' print("First digits for small numbers:")
#' print(extract_leading_digits(data_small, mode = 1)) # Should be 1, 4, 6, 1, 9, 2
#' print("Second digits for small numbers:")
#' print(extract_leading_digits(data_small, mode = 2)) # Should be 2, 5, 7, 0, 8, 0
#'
#' # Example with empty input
#' empty_digits <- extract_leading_digits(numeric(0), mode = 1)
#' print("Digits from empty input:")
#' print(empty_digits)
extract_leading_digits <- function(numbers, mode = 1) {
  # Input validation
  if (!is.numeric(numbers)) {
    stop("Error: 'numbers' must be a numeric vector.")
  }
  if (!all(numbers > 0)) {
    warning("Warning: Input numbers should be positive for Benford's Law analysis. Negative or zero values might yield unexpected results.")
    # Filter out non-positive numbers for the digit extraction
    numbers <- numbers[numbers > 0]
  }
  if (length(numbers) == 0) {
    if (mode == 3) {
      return(list(first_digits = numeric(0), second_digits = numeric(0)))
    } else {
      return(numeric(0))
    }
  }
  
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first and second digits).")
  }
  
  # Convert numbers to character strings to extract digits
  # Using scientific = FALSE to prevent scientific notation for very large/small numbers
  # which could affect digit extraction.
  numbers_char <- format(numbers, scientific = FALSE, trim = TRUE)
  
  # Function to get the first digit
  get_first_digit <- function(s) {
    # Remove leading zeros after the decimal point for numbers < 1 (e.g., "0.00123" -> "1")
    s <- gsub("^0+\\.0*", "", s) # Remove "0." or "0.00" part
    
    # Remove non-digit characters (like '.') and then take the first character
    s <- gsub("\\D", "", s) # Remove non-digits
    if (nchar(s) == 0) {
      return(NA) # Handle cases where string becomes empty after removal
    }
    as.numeric(substr(s, 1, 1))
  }
  
  # Function to get the second digit
  get_second_digit <- function(s) {
    # Remove leading zeros after the decimal point for numbers < 1 (e.g., "0.00123" -> "123")
    s <- gsub("^0+\\.0*", "", s) # Remove "0." or "0.00" part
    
    # Remove non-digit characters (like '.')
    s <- gsub("\\D", "", s) # Remove non-digits
    
    if (nchar(s) < 2) {
      return(NA) # Not enough digits for a second digit
    }
    as.numeric(substr(s, 2, 2))
  }
  
  first_digits <- NULL
  second_digits <- NULL
  
  if (mode == 1 || mode == 3) {
    first_digits <- sapply(numbers_char, get_first_digit, USE.NAMES = FALSE)
    first_digits <- first_digits[!is.na(first_digits)] # Remove NAs
  }
  
  if (mode == 2 || mode == 3) {
    second_digits <- sapply(numbers_char, get_second_digit, USE.NAMES = FALSE)
    second_digits <- second_digits[!is.na(second_digits)] # Remove NAs
  }
  
  # Return based on mode
  if (mode == 1) {
    return(first_digits)
  } else if (mode == 2) {
    return(second_digits)
  } else if (mode == 3) {
    return(list(first_digits = first_digits, second_digits = second_digits))
  }
}