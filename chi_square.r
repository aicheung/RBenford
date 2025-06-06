#' Performs a Chi-squared test to compare observed digit frequencies with Benford's Law distribution.
#'
#' This function calculates the Chi-squared test statistic and p-value to assess
#' whether the observed frequencies of leading digits in a dataset conform to
#' the expected frequencies predicted by Benford's Law.
#'
#' @param observed_digits A numeric vector of the extracted leading digits
#'                        (e.g., first digits, second digits, or first two digits combined).
#' @param mode An integer specifying the Benford distribution to compare against:
#'   1: First digit (digits 1-9).
#'   2: Second digit (digits 0-9).
#'   3: First two digits (digits 10-99).
#' @return A list containing:
#'   \itemize{
#'     \item \code{chi_squared_statistic}: The calculated Chi-squared test statistic.
#'     \item \code{degrees_of_freedom}: The degrees of freedom for the test.
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{observed_counts}: A named numeric vector of observed counts.
#'     \item \code{expected_counts}: A named numeric vector of expected counts.
#'     \item \code{comparison_table}: A data frame showing Digit, Observed Counts, Expected Counts, and (O-E)^2/E.
#'   }
#'   Returns NULL if there are insufficient data points after filtering.
#' @examples
#' # Example 1: First digit test
#' # Simulated data somewhat conforming to Benford's first digit
#' set.seed(123)
#' data_conforming <- c(sample(1:9, 1000, replace = TRUE, prob = generate_benford_distribution(1)),
#'                      rnorm(500, mean = 5000, sd = 2000))
#' # Clean and extract first digits
#' cleaned_data <- data_conforming[data_conforming > 0] # Simple clean for example
#' first_digits <- extract_leading_digits(cleaned_data, mode = 1)
#'
#' chi_sq_result_d1 <- benford_chi_squared_test(first_digits, mode = 1)
#' print("Chi-squared Test for First Digits:")
#' print(chi_sq_result_d1)
#' # Expected p-value to be high if data is good
#'
#' # Example 2: Second digit test (needs more diverse data for good test)
#' # Using the same cleaned_data, but extracting second digits
#' second_digits <- extract_leading_digits(cleaned_data, mode = 2)
#' chi_sq_result_d2 <- benford_chi_squared_test(second_digits, mode = 2)
#' print("\nChi-squared Test for Second Digits:")
#' print(chi_sq_result_d2)
#'
#' # Example 3: First two digits test
#' # Re-creating data for first two digits test
#' two_digits_list <- extract_leading_digits(cleaned_data, mode = 3)
#' valid_indices <- !is.na(two_digits_list$first_digits) & !is.na(two_digits_list$second_digits)
#' first_two_digits <- two_digits_list$first_digits[valid_indices] * 10 + two_digits_list$second_digits[valid_indices]
#'
#' chi_sq_result_d1d2 <- benford_chi_squared_test(first_two_digits, mode = 3)
#' print("\nChi-squared Test for First Two Digits:")
#' print(chi_sq_result_d1d2)
#'
#' # Example 4: Data that might not conform (e.g., all numbers start with 7)
#' non_benford_data <- c(71, 72, 73, 74, 75, 76, 77, 78, 79, 70) * 100
#' cleaned_non_benford <- non_benford_data[non_benford_data > 0]
#' first_digits_non_benford <- extract_leading_digits(cleaned_non_benford, mode = 1)
#' chi_sq_result_non_benford <- benford_chi_squared_test(first_digits_non_benford, mode = 1)
#' print("\nChi-squared Test for Non-Benford Data (should have low p-value):")
#' print(chi_sq_result_non_benford)
benford_chi_squared_test <- function(observed_digits, mode) {
  # Input validation
  if (!is.numeric(observed_digits)) {
    stop("Error: 'observed_digits' must be a numeric vector.")
  }
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first two digits).")
  }
  
  n_observations <- length(observed_digits)
  if (n_observations == 0) {
    warning("No observed digits provided. Chi-squared test cannot be performed.")
    return(NULL)
  }
  
  # 1. Get the theoretical Benford distribution based on mode
  benford_expected_probs <- switch(
    mode,
    "1" = generate_benford_distribution(mode = 1),
    "2" = generate_benford_distribution(mode = 2),
    "3" = generate_benford_distribution_first_two_digits()
  )
  
  # Ensure the levels for observed counts match the expected probabilities
  if (mode == 1) {
    all_possible_digits <- as.character(1:9)
  } else if (mode == 2) {
    all_possible_digits <- as.character(0:9)
  } else { # mode == 3
    all_possible_digits <- as.character(10:99)
  }
  
  # 2. Calculate observed counts for each digit/digit pair
  # Use factor to ensure all categories are present, even if count is 0
  observed_counts_table <- table(factor(observed_digits, levels = all_possible_digits))
  observed_counts <- as.numeric(observed_counts_table)
  names(observed_counts) <- names(observed_counts_table)
  
  # Filter out observed digits that are not valid for the selected mode
  # (This shouldn't happen if extract_leading_digits is used correctly, but adds robustness)
  observed_counts <- observed_counts[names(benford_expected_probs)]
  
  
  # 3. Calculate expected counts based on total observations
  expected_counts <- benford_expected_probs * n_observations
  
  # Chi-squared test requirement: Expected counts should not be too small.
  # A common rule of thumb is that no more than 20% of expected counts are less than 5,
  # and all expected counts are greater than 1.
  # If any expected count is 0, the formula breaks.
  if (any(expected_counts < 1)) {
    warning("Some expected counts are less than 1. Chi-squared approximation may be invalid. Consider grouping categories or using a different test (e.g., Z-score test for first digits) if counts are very low.")
  }
  
  # 4. Calculate the Chi-squared statistic
  # We use the formula (Observed - Expected)^2 / Expected
  # Ensure division by zero is handled if an expected count is zero
  # (though with Benford's formula and sufficient n_observations, expected_counts should not be zero)
  chi_squared_components <- (observed_counts - expected_counts)^2 / expected_counts
  chi_squared_statistic <- sum(chi_squared_components)
  
  # 5. Determine degrees of freedom
  # df = (number of categories - 1)
  degrees_of_freedom <- length(benford_expected_probs) - 1
  
  # 6. Calculate the p-value
  # p-value is the probability of observing a chi-squared statistic as extreme
  # or more extreme than the calculated one, assuming the null hypothesis is true.
  p_value <- pchisq(chi_squared_statistic, df = degrees_of_freedom, lower.tail = FALSE)
  
  # 7. Create a comparison table for detailed output
  comparison_table <- data.frame(
    Digit = names(observed_counts),
    Observed_Counts = observed_counts,
    Expected_Counts = expected_counts,
    Chi_Squared_Component = chi_squared_components,
    row.names = NULL
  )
  
  return(list(
    chi_squared_statistic = chi_squared_statistic,
    degrees_of_freedom = degrees_of_freedom,
    p_value = p_value,
    observed_counts = observed_counts,
    expected_counts = expected_counts,
    comparison_table = comparison_table
  ))
}