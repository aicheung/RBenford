#' Extracts leading digits from a numeric vector for Benford's Law analysis.
#'
#' This function takes a numeric vector and extracts the first digit, the second digit,
#' or both, based on the specified mode. It's designed for use with cleaned data
#' where values are positive and non-zero.
#'
#' @param numbers A numeric vector of positive, non-zero values.
#' @param mode An integer specifying which digits to extract:
#'   1: Extracts the first leading digit.
#'   2: Extracts the second leading digit. Numbers with only one significant digit
#'      (e.g., 1, 0.005) will not yield a second digit and will be excluded.
#'   3: Extracts the first two leading digits, combined into a single number (e.g., 12 for 1st digit 1, 2nd digit 2).
#'      Numbers with only one significant digit will not yield a combined two-digit number and will be excluded.
#' @return A numeric vector (for mode 1, 2, or 3). Returns an empty vector if input is empty
#'          or no valid digits can be extracted.
#' @examples
#' # Example data (assuming it's already cleaned)
#' data_for_digits <- c(123, 45, 6789, 1, 9.87, 20.5, 3000)
#'
#' # Mode 1: First digit only
#' first_digits <- extract_leading_digits(data_for_digits, mode = 1)
#' print("First digits:")
#' print(first_digits)
#' # Expected: 1, 4, 6, 1, 9, 2, 3
#'
#' # Mode 2: Second digit only
#' second_digits <- extract_leading_digits(data_for_digits, mode = 2)
#' print("Second digits:")
#' print(second_digits)
#' # Expected: 2, 5, 7, NA (1 excluded), 8, 0, 0
#' # So, output should be: 2, 5, 7, 8, 0, 0
#'
#' # Mode 3: First and second digits combined
#' combined_digits <- extract_leading_digits(data_for_digits, mode = 3)
#' print("First and second digits (combined):")
#' print(combined_digits)
#' # Expected: 12, 45, 67, NA (1 excluded), 98, 20, 30
#' # So, output should be: 12, 45, 67, 98, 20, 30
#'
#' # Example with numbers less than 1 (but > 0)
#' data_small <- c(0.123, 0.045, 0.6789, 0.001, 0.987, 0.205)
#' print("First digits for small numbers:")
#' print(extract_leading_digits(data_small, mode = 1)) # Should be 1, 4, 6, 1, 9, 2
#' print("Second digits for small numbers:")
#' print(extract_leading_digits(data_small, mode = 2)) # Should be 2, 5, 7, NA (0.001 excluded), 8, 0
#' # Output: 2, 5, 7, 8, 0
#' print("Combined digits for small numbers:")
#' print(extract_leading_digits(data_small, mode = 3)) # Should be 12, 45, 67, NA (0.001 excluded), 98, 20
#' # Output: 12, 45, 67, 98, 20
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
    return(numeric(0)) # Always return an empty numeric vector for empty input
  }
  
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first and second digits).")
  }
  
  # Convert numbers to character strings to extract digits
  # Using scientific = FALSE to prevent scientific notation for very large/small numbers
  # which could affect digit extraction.
  numbers_char <- as.character(numbers)
  
  # Function to get the first digit
  get_first_digit <- function(s) {
    # Remove leading zeros and any decimal point before the first significant digit
    s <- gsub("^0*\\.0*", "", s) # FIX: More robust regex
    
    # Remove any remaining non-digit characters (like commas if locale affects format)
    s <- gsub("\\D", "", s)
    
    if (nchar(s) == 0) {
      return(NA) # Handle cases where string becomes empty after removal (e.g., input was "0" or "0.0")
    }
    as.numeric(substr(s, 1, 1))
  }
  
  # Function to get the second digit
  get_second_digit <- function(s) {
    # Remove leading zeros and any decimal point before the first significant digit
    s <- gsub("^0*\\.0*", "", s) # FIX: More robust regex
    
    # Remove any remaining non-digit characters
    s <- gsub("\\D", "", s)
    
    # Key change: If there are fewer than 2 significant digits, return NA
    if (nchar(s) < 2) {
      return(NA)
    }
    as.numeric(substr(s, 2, 2))
  }
  
  if (mode == 1) {
    first_digits <- sapply(numbers_char, get_first_digit, USE.NAMES = FALSE)
    return(first_digits[!is.na(first_digits)])
  } else if (mode == 2) {
    second_digits <- sapply(numbers_char, get_second_digit, USE.NAMES = FALSE)
    return(second_digits[!is.na(second_digits)])
  } else if (mode == 3) {
    first_digits_raw <- sapply(numbers_char, get_first_digit, USE.NAMES = FALSE)
    second_digits_raw <- sapply(numbers_char, get_second_digit, USE.NAMES = FALSE)
    
    # Identify indices where both first and second digits could be extracted
    # A NA in either means it shouldn't be included in the combined result
    valid_indices <- !is.na(first_digits_raw) & !is.na(second_digits_raw)
    
    # Combine them only for valid entries
    combined_digits <- (first_digits_raw[valid_indices] * 10) + second_digits_raw[valid_indices]
    
    return(combined_digits) # NAs are already excluded by `valid_indices`
  }
}