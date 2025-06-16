# Helper function to get the nearest critical value from Table 8
get_excess_mad_critical_value <- function(sample_size, mode, percentile = 95) {
  # Data from Table 8 (95th percentile values)
  # For First Digit
  d1_95_percentiles <- data.frame(
    N = c(100, 200, 300, 400, 500, 1000, 2000, 3000, 3500),
    Value = c(0.011396, 0.008068, 0.006535, 0.005755, 0.005171, 0.003620, 0.002592, 0.002110, 0.001945)
  )
  
  # For Second Digit
  d2_95_percentiles <- data.frame(
    N = c(100, 200, 300, 400, 500, 1000, 2000, 3000, 3500),
    Value = c(0.010464, 0.007267, 0.006031, 0.005139, 0.004663, 0.003344, 0.002273, 0.001907, 0.001742)
  )
  
  # For First-Two Digits
  d1d2_95_percentiles <- data.frame(
    N = c(100, 200, 300, 400, 500, 1000, 2000, 3000, 3500),
    Value = c(0.001004, 0.000764, 0.000627, 0.000558, 0.000500, 0.000351, 0.000246, 0.000203, 0.000188)
  )
  
  # Select the correct table based on mode
  percentile_data <- switch(
    mode,
    "1" = d1_95_percentiles,
    "2" = d2_95_percentiles,
    "3" = d1d2_95_percentiles,
    stop("Invalid mode for critical value lookup.")
  )
  
  # Find the closest sample size in the table and use its corresponding value
  # Or, for more precision, use approx() for linear interpolation
  # Here, we'll implement simple lookup for the closest N
  
  if (sample_size < min(percentile_data$N)) {
    warning(paste("Sample size", sample_size, "is below the minimum in Table 8. Using critical value for N =", min(percentile_data$N), "."))
    closest_value <- percentile_data$Value[which.min(abs(percentile_data$N - min(percentile_data$N)))]
  } else if (sample_size > max(percentile_data$N)) {
    warning(paste("Sample size", sample_size, "is above the maximum in Table 8. Using critical value for N =", max(percentile_data$N), "."))
    closest_value <- percentile_data$Value[which.min(abs(percentile_data$N - max(percentile_data$N)))]
  } else {
    # For values within the table range, use approximation to interpolate
    closest_value <- approx(x = percentile_data$N, y = percentile_data$Value, xout = sample_size)$y
  }
  
  return(closest_value)
}


#' Calculates the ExcessMAD (Mean Absolute Deviation adjusted for sample size)
#' between observed digit frequencies and Benford's Law expected frequencies,
#' using sample-size-dependent critical values from Silva and Gouvea (2023).
#'
#' ExcessMAD = MAD - E(MAD), where E(MAD) is the expected MAD for a Benford
#' distribution of the given sample size. If ExcessMAD <= the specified percentile
#' critical value, the data is considered to conform to Benford's Law.
#'
#' @param observed_digits A numeric vector of the extracted leading digits
#'                        (e.g., first digits, second digits, or first two digits combined).
#' @param mode An integer specifying the Benford distribution:
#'   1: First digit (digits 1-9).
#'   2: Second digit (digits 0-9).
#'   3: First two digits (digits 10-99).
#' @param percentile The percentile to use as the critical value (e.g., 90, 95, 99).
#'                   Currently, only 95th percentile values are hardcoded from Table 8.
#' @return A list containing:
#'   \itemize{
#'     \item \code{excess_mad_value}: The calculated ExcessMAD value.
#'     \item \code{mad_value}: The original MAD value.
#'     \item \code{expected_mad_value}: The calculated E(MAD) value for the sample size.
#'     \item \code{critical_value}: The ExcessMAD critical value from Table 8 for the given N and percentile.
#'     \item \code{conformity_conclusion}: A character string indicating conformity based on the critical value.
#'   }
#'   Returns NULL if there are insufficient data points or if E(MAD) cannot be calculated.
#' @references
#' Silva, A. de A. and Gouvea, M.A. (2023) 'Study on the effect of sample size on type I error,
#' in the first, second and first-two digits excessmad tests', International Journal of Accounting
#' Information Systems, 48, 100599. https://doi.org/10.1016/j.accinf.2022.100599
#' @examples
#' # (Assuming clean_benford_data, extract_leading_digits, benford_mad_test,
#' # generate_benford_distribution, and generate_benford_distribution_first_two_digits are sourced)
#'
#' # Example data
#' set.seed(789)
#' data_conforming <- c(sample(1:9, 1500, replace = TRUE, prob = generate_benford_distribution(1)),
#'                      rnorm(1000, mean = 5000, sd = 2000))
#' cleaned_data <- clean_benford_data(data_conforming)
#'
#' if (length(cleaned_data) > 0) {
#'   # ExcessMAD for First Digits (N=2500) with 95th percentile cutoff
#'   first_digits <- extract_leading_digits(cleaned_data, mode = 1)
#'   excess_mad_d1 <- benford_excess_mad_test(first_digits, mode = 1, percentile = 95)
#'   print("ExcessMAD Test for First Digits (using 95th percentile cutoff):")
#'   print(excess_mad_d1)
#'
#'   # ExcessMAD for First Two Digits (N=2500, needs enough data for 2 digits)
#'   extracted_list_d1d2 <- extract_leading_digits(cleaned_data, mode = 3)
#'   valid_indices_d1d2 <- !is.na(extracted_list_d1d2$first_digits) & !is.na(extracted_list_d1d2$second_digits)
#'   first_two_digits <- extracted_list_d1d2$first_digits[valid_indices_d1d2] * 10 +
#'                       extracted_list_d1d2$second_digits[valid_indices_d1d2]
#'
#'   if (length(first_two_digits) > 0) {
#'     excess_mad_d1d2 <- benford_excess_mad_test(first_two_digits, mode = 3, percentile = 95)
#'     print("\nExcessMAD Test for First Two Digits (using 95th percentile cutoff):")
#'     print(excess_mad_d1d2)
#'   } else {
#'     print("\nNot enough data for First Two Digits ExcessMAD test.")
#'   }
#'   
#'   # Example with a smaller sample size (e.g., 250 elements) to test interpolation/extrapolation
#'   set.seed(111)
#'   small_data <- c(sample(1:9, 250, replace = TRUE, prob = generate_benford_distribution(1)))
#'   small_cleaned_data <- clean_benford_data(small_data)
#'   small_first_digits <- extract_leading_digits(small_cleaned_data, mode = 1)
#'   
#'   if (length(small_first_digits) > 0) {
#'     excess_mad_small_d1 <- benford_excess_mad_test(small_first_digits, mode = 1, percentile = 95)
#'     print("\nExcessMAD Test for First Digits (Small Sample N=250):")
#'     print(excess_mad_small_d1)
#'   }
#' }
benford_excess_mad_test <- function(observed_digits, mode, percentile = 95) {
  # Input validation
  if (!is.numeric(observed_digits)) {
    stop("Error: 'observed_digits' must be a numeric vector.")
  }
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first two digits).")
  }
  if (!percentile %in% c(90, 95, 99)) {
    # If other percentiles are needed, their data from Table 8 must be hardcoded as well.
    stop("Error: 'percentile' must be 90, 95, or 99. Currently only 95th percentile values are implemented.")
  }
  
  n_observations <- length(observed_digits)
  if (n_observations == 0) {
    warning("No observed digits provided. ExcessMAD test cannot be performed.")
    return(NULL)
  }
  
  # 1. Calculate the standard MAD value for the observed data
  mad_result <- benford_mad_test(observed_digits, mode)
  if (is.null(mad_result)) {
    warning("Could not calculate MAD for observed digits. ExcessMAD test cannot be performed.")
    return(NULL)
  }
  mad_value <- mad_result$mad_value
  
  # 2. Calculate E(MAD) based on mode and sample size (N) using the approximate formulas
  # The paper standardizes to N=1000 for approx in their simulation, but theoretical min N for approx is lower.
  # We will use the approximate formulas from the paper directly.
  
  expected_mad_value <- NA # Initialize
  if (mode == 1) { # First Digit
    expected_mad_value <- 1 / sqrt(18.13132 * n_observations)
  } else if (mode == 2) { # Second Digit
    expected_mad_value <- 1 / sqrt(17.51766 * n_observations)
  } else if (mode == 3) { # First Two Digits
    expected_mad_value <- 1 / sqrt(158.83263 * n_observations)
  }
  
  if (is.na(expected_mad_value)) {
    warning("Unexpected issue in calculating Expected MAD. ExcessMAD test cannot be fully performed.")
    return(NULL)
  }
  
  # 3. Calculate ExcessMAD
  excess_mad_value <- mad_value - expected_mad_value
  
  # 4. Get the dynamic critical value from Table 8 based on N and percentile
  # For this implementation, only 95th percentile is hardcoded as per the example.
  # To support 90th and 99th, their respective rows from Table 8 would need to be added to get_excess_mad_critical_value.
  critical_value <- get_excess_mad_critical_value(n_observations, mode, percentile)
  
  # 5. Determine conformity conclusion based on the dynamic critical value
  conformity_conclusion <- if (excess_mad_value <= critical_value) {
    paste0("Conforms to Benford's Law (ExcessMAD <= Critical Value @ ", percentile, "th percentile)")
  } else {
    paste0("Does NOT conform to Benford's Law (ExcessMAD > Critical Value @ ", percentile, "th percentile)")
  }
  
  return(list(
    excess_mad_value = excess_mad_value,
    mad_value = mad_value,
    expected_mad_value = expected_mad_value,
    critical_value = critical_value,
    conformity_conclusion = conformity_conclusion
  ))
}

