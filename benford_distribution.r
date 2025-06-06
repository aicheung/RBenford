#' Generates the theoretical Benford's Law distribution for first or second digits.
#'
#' This function calculates the expected probabilities for the first or second
#' digits according to Benford's Law.
#'
#' @param mode An integer specifying which distribution to generate:
#'   1: First digit distribution (digits 1-9).
#'   2: Second digit distribution (digits 0-9).
#' @return A named numeric vector representing the theoretical Benford probabilities,
#'         or an error if an invalid mode is provided.
#' @examples
#' # Generate first digit Benford distribution
#' benford_d1 <- generate_benford_distribution(mode = 1)
#' print("Benford's Law First Digit Distribution:")
#' print(benford_d1)
#' sum(benford_d1) # Should be approximately 1
#'
#' # Generate second digit Benford distribution
#' benford_d2 <- generate_benford_distribution(mode = 2)
#' print("Benford's Law Second Digit Distribution:")
#' print(benford_d2)
#' sum(benford_d2) # Should be approximately 1
generate_benford_distribution <- function(mode = 1) {
  if (!mode %in% c(1, 2)) {
    stop("Error: 'mode' must be 1 (first digit) or 2 (second digit).")
  }
  
  if (mode == 1) {
    # Benford's Law for First Digit (d1 = 1, ..., 9)
    # P(d1) = log10(1 + 1/d1)
    digits <- 1:9
    probabilities <- log10(1 + 1 / digits)
    names(probabilities) <- as.character(digits)
  } else { # mode == 2
    # Benford's Law for Second Digit (d2 = 0, ..., 9)
    # P(d2) = sum from d1=1 to 9 of log10(1 + 1/(10*d1 + d2))
    # A more direct formula for the second digit is:
    # P(d2) = sum_{k=1}^{9} [log10(1 + (1 / (k*10 + d2)))]
    # This is equivalent to P(d2) = Sum_{i=1 to inf} [log10( (10i + d2 + 1) / (10i + d2) )]
    # A simpler way to calculate it is to sum the probabilities for the first two digits
    # for all first digits, where the second digit is fixed.
    
    digits <- 0:9
    probabilities <- numeric(length(digits))
    names(probabilities) <- as.character(digits)
    
    for (d2 in digits) {
      sum_val <- 0
      for (d1 in 1:9) {
        sum_val <- sum_val + log10(1 + 1 / (d1 * 10 + d2))
      }
      probabilities[as.character(d2)] <- sum_val
    }
  }
  
  # Normalize the probabilities to ensure they sum to 1 (though Benford's formulas naturally do)
  # This is a safeguard, especially with floating point arithmetic.
  # probabilities <- probabilities / sum(probabilities)
  
  return(probabilities)
}

#' Generates the theoretical Benford's Law distribution for the first two digits.
#'
#' This function calculates the expected probabilities for the first two digits
#' (ranging from 10 to 99) according to Benford's Law.
#'
#' @return A named numeric vector representing the theoretical Benford probabilities
#'         for the first two digits. The names of the vector elements will be
#'         the two-digit numbers (e.g., "10", "11", ..., "99").
#' @examples
#' benford_d1d2 <- generate_benford_distribution_first_two_digits()
#' print("Benford's Law First Two Digits Distribution (first few entries):")
#' print(head(benford_d1d2))
#' print("Probability of starting with 15:")
#' print(benford_d1d2["15"])
#' sum(benford_d1d2) # Should be approximately 1
generate_benford_distribution_first_two_digits <- function() {
  # The first two digits combined can range from 10 to 99
  two_digit_numbers <- 10:99
  
  # Calculate probabilities using the Benford's Law formula
  # P(N) = log10(1 + 1/N) where N is the number formed by the first two digits
  probabilities <- log10(1 + 1 / two_digit_numbers)
  
  # Name the probabilities with the corresponding two-digit number
  names(probabilities) <- as.character(two_digit_numbers)
  
  return(probabilities)
}