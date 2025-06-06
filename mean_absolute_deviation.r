#' Calculates the Mean Absolute Deviation (MAD) between observed digit frequencies
#' and Benford's Law expected frequencies.
#'
#' The MAD provides a simple, intuitive measure of the average difference
#' between the actual proportions of leading digits in a dataset and the
#' proportions predicted by Benford's Law. Lower values indicate a better fit.
#'
#' @param observed_digits A numeric vector of the extracted leading digits
#'                        (e.g., first digits, second digits, or first two digits combined).
#' @param mode An integer specifying the Benford distribution to compare against:
#'   1: First digit (digits 1-9).
#'   2: Second digit (digits 0-9).
#'   3: First two digits (digits 10-99).
#' @return A list containing:
#'   \itemize{
#'     \item \code{mad_value}: The calculated Mean Absolute Deviation.
#'     \item \code{comparison_table}: A data frame showing Digit, Observed Frequencies, Expected Frequencies, and Absolute Differences.
#'   }
#'   Returns NULL if there are insufficient data points after filtering.
#' @examples
#' # (Assuming clean_benford_data and extract_leading_digits are sourced)
#' # Example 1: First digit MAD
#' set.seed(123)
#' data_conforming <- c(sample(1:9, 1000, replace = TRUE, prob = generate_benford_distribution(1)),
#'                      rnorm(500, mean = 5000, sd = 2000))
#' cleaned_data <- data_conforming[data_conforming > 0]
#' first_digits <- extract_leading_digits(cleaned_data, mode = 1)
#'
#' mad_result_d1 <- benford_mad_test(first_digits, mode = 1)
#' print("Mean Absolute Deviation for First Digits:")
#' print(mad_result_d1)
#'
#' # Example 2: First two digits MAD
#' two_digits_list <- extract_leading_digits(cleaned_data, mode = 3)
#' valid_indices <- !is.na(two_digits_list$first_digits) & !is.na(two_digits_list$second_digits)
#' first_two_digits <- two_digits_list$first_digits[valid_indices] * 10 + two_digits_list$second_digits[valid_indices]
#'
#' mad_result_d1d2 <- benford_mad_test(first_two_digits, mode = 3)
#' print("\nMean Absolute Deviation for First Two Digits:")
#' print(mad_result_d1d2)
#'
#' # Example 3: Data that might not conform (e.g., all numbers start with 7)
#' non_benford_data <- c(71, 72, 73, 74, 75, 76, 77, 78, 79, 70) * 100
#' cleaned_non_benford <- non_benford_data[non_benford_data > 0]
#' first_digits_non_benford <- extract_leading_digits(cleaned_non_benford, mode = 1)
#' mad_result_non_benford <- benford_mad_test(first_digits_non_benford, mode = 1)
#' print("\nMean Absolute Deviation for Non-Benford Data (should be higher):")
#' print(mad_result_non_benford)
benford_mad_test <- function(observed_digits, mode) {
  # Input validation
  if (!is.numeric(observed_digits)) {
    stop("Error: 'observed_digits' must be a numeric vector.")
  }
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first two digits).")
  }
  
  n_observations <- length(observed_digits)
  if (n_observations == 0) {
    warning("No observed digits provided. MAD test cannot be performed.")
    return(NULL)
  }
  
  # 1. Get the theoretical Benford distribution (probabilities/PMF)
  benford_expected_probs <- switch(
    mode,
    "1" = generate_benford_distribution(mode = 1),
    "2" = generate_benford_distribution(mode = 2),
    "3" = generate_benford_distribution_first_two_digits()
  )
  
  # Define the domain (possible digit values)
  if (mode == 1) {
    all_possible_digits <- as.character(1:9)
  } else if (mode == 2) {
    all_possible_digits <- as.character(0:9)
  } else { # mode == 3
    all_possible_digits <- as.character(10:99)
  }
  
  # 2. Calculate observed frequencies (proportions) for each digit/digit pair
  # Use factor to ensure all categories are present, even if count is 0
  observed_counts_table <- table(factor(observed_digits, levels = all_possible_digits))
  observed_frequencies <- as.numeric(observed_counts_table) / n_observations
  names(observed_frequencies) <- names(observed_counts_table)
  
  # Ensure observed frequencies align with expected probabilities by their names
  observed_frequencies <- observed_frequencies[names(benford_expected_probs)]
  
  
  # 3. Calculate absolute differences
  absolute_differences <- abs(observed_frequencies - benford_expected_probs)
  
  # 4. Calculate MAD
  mad_value <- sum(absolute_differences) / length(benford_expected_probs) # Divide by number of categories
  
  # 5. Create a comparison table for detailed output
  comparison_table <- data.frame(
    Digit = names(observed_frequencies),
    Observed_Frequency = observed_frequencies,
    Expected_Frequency = benford_expected_probs,
    Absolute_Difference = absolute_differences,
    row.names = NULL
  )
  
  return(list(
    mad_value = mad_value,
    comparison_table = comparison_table
  ))
}