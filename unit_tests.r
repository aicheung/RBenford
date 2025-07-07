library(testthat)

source("clean_benford_data.r")
source("excess_mean_absolute_deviation.r")
source("mean_absolute_deviation.r")
source("extract_digits.r")
source("benford_distribution.r")
source("chi_square.r")
source("visualisation.r")

test_that("Input validation works correctly", {
  # Non-numeric input
  expect_error(extract_leading_digits(c("a", "b")), "must be a numeric vector")
  
  # Invalid mode
  expect_error(extract_leading_digits(c(123), mode = 4))
  
  # Zero/negative handling
  expect_warning(extract_leading_digits(c(-5, 0, 10)), "positive for Benford")
  expect_equal(suppressWarnings(extract_leading_digits(c(-5, 0, 10), mode = 1)), 1)
})

test_that("First digit extraction (mode=1) works correctly including single-digit numbers and small numbers", {
  numbers <- c(123, 45, 1, 9.87, 20.5, 3000, 0.123, 0.045, 0.6789, 0.001)
  result <- extract_leading_digits(numbers, mode = 1)
  expect_equal(result, c(1, 4, 1, 9, 2, 3, 1, 4, 6, 1))
})

test_that("Second digit extraction (mode=2) works correctly and excludes single-digit numbers", {
  numbers <- c(123, 45, 1, 9.87, 20.5, 3000, 0.123, 0.045, 0.6789, 0.001)
  result <- extract_leading_digits(numbers, mode = 2)
  # 123 -> 2
  # 45  -> 5
  # 1   -> EXCLUDED (NA)
  # 9.87 -> 8
  # 20.5 -> 0
  # 3000 -> 0
  # 0.123 -> 2
  # 0.045 -> 5 (significant digits are 45)
  # 0.6789 -> 7
  # 0.001 -> EXCLUDED (NA)
  expect_equal(result, c(2, 5, 8, 0, 0, 2, 5, 7))
})

test_that("Dual digit extraction (mode=3) works correctly and returns combined digits, excluding single-digit numbers", {
  numbers <- c(123, 45, 1, 9.87, 20.5, 3000, 0.123, 0.045, 0.6789, 0.001)
  result <- extract_leading_digits(numbers, mode = 3)
  # 123 -> 12
  # 45  -> 45
  # 1   -> EXCLUDED (NA)
  # 9.87 -> 98
  # 20.5 -> 20
  # 3000 -> 30
  # 0.123 -> 12
  # 0.045 -> 45
  # 0.6789 -> 67
  # 0.001 -> EXCLUDED (NA)
  expect_equal(result, c(12, 45, 98, 20, 30, 12, 45, 67))
})

test_that("extract_leading_digits handles empty input correctly across all modes", {
  expect_equal(extract_leading_digits(numeric(0), mode = 1), numeric(0))
  expect_equal(extract_leading_digits(numeric(0), mode = 2), numeric(0))
  expect_equal(extract_leading_digits(numeric(0), mode = 3), numeric(0))
})

test_that("extract_leading_digits handles non-positive numbers by filtering and returns appropriate digits", {
  numbers <- c(-123, 0, 45, 1, 987, -5, 0.00012)
  # Filtered: 45, 1, 987, 0.00012
  
  # Mode 1:
  # 45 -> 4
  # 1  -> 1
  # 987 -> 9
  # 0.00012 -> 1
  suppressWarnings(expect_equal(extract_leading_digits(numbers, mode = 1), c(4, 1, 9, 1)))
  
  # Mode 2:
  # 45 -> 5
  # 1  -> EXCLUDED
  # 987 -> 8
  # 0.00012 -> 2
  suppressWarnings(expect_equal(extract_leading_digits(numbers, mode = 2), c(5, 8, 2)))
  
  # Mode 3:
  # 45 -> 45
  # 1  -> EXCLUDED
  # 987 -> 98
  # 0.00012 -> 12
  suppressWarnings(expect_equal(extract_leading_digits(numbers, mode = 3), c(45, 98, 12)))
})

test_that("extract_leading_digits handles numbers that become single-digit after cleaning (e.g., 0.000000001)", {
  numbers <- c(1.0, 0.000000001, 123456789)
  
  # Mode 1:
  # 1.0 -> 1
  # 0.000000001 -> 1
  # 123456789 -> 1
  expect_equal(extract_leading_digits(numbers, mode = 1), c(1, 1, 1))
  
  # Mode 2:
  # 1.0 -> 0
  # 0.000000001 -> EXCLUDED (becomes "1" after cleaning, no second digit)
  # 123456789 -> 2
  expect_equal(extract_leading_digits(numbers, mode = 2), c(0, 2))
  
  # Mode 3:
  # 1.0 -> 10
  # 0.000000001 -> EXCLUDED
  # 123456789 -> 12
  expect_equal(extract_leading_digits(numbers, mode = 3), c(10, 12))
})

test_that("extract_leading_digits warns for non-positive numbers", {
  expect_warning(extract_leading_digits(c(1, -2, 0, 3), mode = 1), "Warning: Input numbers should be positive")
})

test_that("extract_leading_digits stops for non-numeric input", {
  expect_error(extract_leading_digits("abc", mode = 1), "Error: 'numbers' must be a numeric vector.")
})

test_that("extract_leading_digits stops for invalid mode", {
  expect_error(extract_leading_digits(c(10, 20), mode = 4), "Error: 'mode' must be 1 \\(first digit\\), 2 \\(second digit\\), or 3 \\(first and second digits\\).")
})

test_that("generate_benford_distribution input validation works", {
  # Invalid mode
  expect_error(generate_benford_distribution(mode = 3))
  expect_error(generate_benford_distribution(mode = "a"))
})

test_that("First digit distribution (mode=1) is correct", {
  d1 <- generate_benford_distribution(mode = 1)
  
  # Verify digit range
  expect_equal(names(d1), as.character(1:9))
  
  # Verify probabilities
  expected <- log10(1 + 1/1:9)
  names(expected) <- 1:9
  expect_equal(d1, expected)
  
  # Verify sum is approximately 1
  expect_equal(sum(d1), 1, tolerance = 1e-7)
  
  # Spot check specific values
  expect_equal(unname(d1["1"]), log10(2), tolerance = 0.1)
  expect_equal(unname(d1["9"]), log10(10/9), tolerance = 1e-1)
})

test_that("Second digit distribution (mode=2) is correct", {
  d2 <- generate_benford_distribution(mode = 2)
  
  # Verify digit range
  expect_equal(names(d2), as.character(0:9))
  
  # Verify probabilities
  expected <- sapply(0:9, function(d2) {
    sum(sapply(1:9, function(d1) log10(1 + 1/(10*d1 + d2))))
  })
  names(expected) <- 0:9
  expect_equal(d2, expected)
  
  # Verify sum is approximately 1
  expect_equal(sum(d2), 1, tolerance = 1e-7)
  
  # Spot check specific values
  expect_equal(d2["0"], expected["0"], tolerance = 1e-7)
  expect_equal(d2["5"], expected["5"], tolerance = 1e-7)
})

test_that("First two-digit distribution is correct", {
  d12 <- generate_benford_distribution_first_two_digits()
  
  # Verify digit range
  expect_equal(names(d12), as.character(10:99))
  
  # Verify probabilities
  expected <- log10(1 + 1/10:99)
  names(expected) <- 10:99
  expect_equal(d12, expected)
  
  # Verify sum is approximately 1
  expect_equal(sum(d12), 1, tolerance = 1e-7)
  
  # Spot check specific values
  expect_equal(unname(d12["10"]), log10(1 + 1/10), tolerance = 1e-7)
  expect_equal(unname(d12["99"]), log10(1 + 1/99), tolerance = 1e-7)
  
  # Verify all values are within expected range
  expect_true(all(d12 > 0))
  expect_true(all(d12 < 0.05))  # No probability > 5% for two-digit numbers
})

test_that("Distribution properties hold", {
  # First digit distribution should have decreasing probabilities
  d1 <- generate_benford_distribution(mode = 1)
  expect_true(all(diff(d1) < 0))  # Each digit should be less probable than previous
  
  # Second digit distribution should have 0 as most common
  d2 <- generate_benford_distribution(mode = 2)
  expect_equal(unname(which.max(d2)), 1)  # Position 1 (digit 0) should be max
  
  # First two-digit distribution should be decreasing
  d12 <- generate_benford_distribution_first_two_digits()
  expect_true(all(diff(d12) < 0))  # Decreasing with higher numbers
})

test_that("benford_chi_squared_test works as expected", {
  # Generate first digit Benford distribution for sampling
  benford_probs <- generate_benford_distribution(mode = 1)
  
  # Create a sample conforming dataset
  set.seed(42)
  conforming_digits <- sample(1:9, size = 1000, replace = TRUE, prob = benford_probs)
  
  # Run chi-squared test
  result_conforming <- benford_chi_squared_test(conforming_digits, mode = 1)
  
  # Expect p-value not too small (since data should conform)
  expect_true(result_conforming$p_value > 0.05)
  
  # Non-conforming data: all leading digits are 9
  non_conforming_digits <- rep(9, 100)
  result_nonconforming <- benford_chi_squared_test(non_conforming_digits, mode = 1)
  
  # Expect p-value very small (since distribution is very biased)
  expect_true(result_nonconforming$p_value < 0.01)
  
  # Test output structure
  expect_named(result_conforming, c(
    "chi_squared_statistic",
    "degrees_of_freedom",
    "p_value",
    "observed_counts",
    "expected_counts",
    "comparison_table"
  ))
  
  expect_s3_class(result_conforming$comparison_table, "data.frame")
  
  # Degrees of freedom for first digits (1-9): 9 - 1 = 8
  expect_equal(result_conforming$degrees_of_freedom, 8)
  
  # Observed counts should be numeric vector of length 9
  expect_type(result_conforming$observed_counts, "double")
  expect_length(result_conforming$observed_counts, 9)
  
  # Handling of empty input
  expect_warning(result_empty <- benford_chi_squared_test(numeric(0), mode = 1))
  expect_null(result_empty)
  
  # Invalid mode should error
  expect_error(benford_chi_squared_test(conforming_digits, mode = 99))
  
  # Invalid input type should error
  expect_error(benford_chi_squared_test(as.character(conforming_digits), mode = 1))
})

test_that("clean_benford_data correctly cleans a mixed CSV file", {
  # Create a temporary CSV file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "Amount",
    "123.45",
    "0.99",
    "0123",
    "456",
    "",
    "   ",
    "0",
    "789.00",
    "abc",
    "  543.21  "
  ), con = temp_file)
  
  # Call the function
  cleaned <- suppressWarnings(clean_benford_data(temp_file))
  
  # Expected: removes 0123 (leading zero), "", "   ", "0", "abc"
  # Keeps 123.45, 0.99, 456, 789, 543.21
  expected <- c(123.45, 0.99, 456, 789.00, 543.21)
  
  expect_equal(cleaned, expected)
  
  # Clean up
  unlink(temp_file)
})

test_that("clean_benford_data returns empty numeric vector if no valid data", {
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "Amount",
    "0",
    " ",
    "",
    "0",
    "abc",
    "0123"
  ), con = temp_file)
  
  expect_warning(result <- clean_benford_data(temp_file), 
                 "No valid numeric data remaining after cleaning")
  
  expect_type(result, "double")
  expect_length(result, 0)
  
  unlink(temp_file)
})

test_that("clean_benford_data errors if file does not exist", {
  fake_path <- tempfile()
  expect_error(clean_benford_data(fake_path), 
               regexp = "Error: File not found")
})

test_that("clean_benford_data errors if CSV has more than one column", {
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "A,B",
    "1,2",
    "3,4"
  ), con = temp_file)
  
  expect_error(clean_benford_data(temp_file), 
               regexp = "The CSV file must contain exactly one column")
  
  unlink(temp_file)
})


test_that("get_excess_mad_critical_value returns expected interpolated values", {
  # Test exact N match (e.g., N=1000, mode=1)
  val1 <- get_excess_mad_critical_value(1000, "1")
  expect_equal(val1, 0.003620, tolerance = 1e-6)
  
  # Test interpolation between N=1000 and N=2000
  interpolated_val <- get_excess_mad_critical_value(1500, "1")
  expect_true(interpolated_val > 0.002592 && interpolated_val < 0.003620)
  
  # Test below min N
  expect_warning(
    val_low <- get_excess_mad_critical_value(50, "1")
  )
  expect_equal(val_low, 0.011396, tolerance = 1e-6)
  
  # Test above max N
  expect_warning(
    val_high <- get_excess_mad_critical_value(5000, "2")
  )
  expect_equal(val_high, 0.001742, tolerance = 1e-6)
  
  # Test invalid mode
  expect_error(
    get_excess_mad_critical_value(1000, "99")
  )
})

test_that("benford_excess_mad_test computes correct output structure and types", {
  # Generate a synthetic dataset conforming to Benford's Law (first digits)
  set.seed(123)
  digits <- sample(1:9, 500, replace = TRUE, prob = generate_benford_distribution(1))
  
  # Run test
  res <- benford_excess_mad_test(digits, mode = 1, percentile = 95)
  
  # Check result is a list
  expect_type(res, "list")
  
  # Check that all expected fields are present
  expect_named(res, c("excess_mad_value", "mad_value", "expected_mad_value",
                      "critical_value", "conformity_conclusion"))
  
  # Check types
  expect_type(res$excess_mad_value, "double")
  expect_type(res$mad_value, "double")
  expect_type(res$expected_mad_value, "double")
  expect_type(res$critical_value, "double")
  expect_type(res$conformity_conclusion, "character")
})

test_that("benford_excess_mad_test handles invalid inputs", {
  # Non-numeric digits
  expect_error(
    benford_excess_mad_test(c("a", "b"), mode = 1)
  )
  
  # Invalid mode
  expect_error(
    benford_excess_mad_test(1:10, mode = 99)
  )
  
  # Invalid percentile
  expect_error(
    benford_excess_mad_test(1:10, mode = 1, percentile = 80)
  )
  
  # Empty input vector
  expect_warning(
    res_empty <- benford_excess_mad_test(numeric(0), mode = 1)
  )
  expect_null(res_empty)
})

test_that("benford_excess_mad_test behaves correctly with small sample sizes", {
  set.seed(456)
  small_digits <- sample(1:9, 50, replace = TRUE, prob = generate_benford_distribution(1))
  
  suppressWarnings(res_small <- benford_excess_mad_test(small_digits, mode = 1, percentile = 95))
  
  expect_type(res_small, "list")
  expect_true(res_small$critical_value > 0)
})

test_that("benford_excess_mad_test behaves correctly with large sample sizes", {
  set.seed(789)
  large_digits <- sample(1:9, 4000, replace = TRUE, prob = generate_benford_distribution(1))
  
  suppressWarnings(res_large <- benford_excess_mad_test(large_digits, mode = 1, percentile = 95))
  
  expect_type(res_large, "list")
  expect_true(res_large$critical_value > 0)
})

test_that("benford_excess_mad_test computes expected MAD and excess MAD correctly", {
  # For controlled test, use uniform digits to get high MAD
  uniform_digits <- rep(1:9, length.out = 500)
  
  res_uniform <- benford_excess_mad_test(uniform_digits, mode = 1, percentile = 95)
  
  expect_true(res_uniform$mad_value > 0)
  expect_true(res_uniform$expected_mad_value > 0)
  expect_equal(res_uniform$excess_mad_value,
               res_uniform$mad_value - res_uniform$expected_mad_value,
               tolerance = 1e-8)
})

# test_benford_mad_test.R

test_that("benford_mad_test computes MAD correctly for first digits", {
  set.seed(123)
  
  # Simulated data conforming to Benford's law
  benford_probs <- generate_benford_distribution(mode = 1)
  benford_digits <- sample(1:9, size = 1000, replace = TRUE, prob = benford_probs)
  
  result <- benford_mad_test(benford_digits, mode = 1)
  
  expect_type(result, "list")
  expect_named(result, c("mad_value", "comparison_table"))
  expect_true(is.numeric(result$mad_value))
  expect_true(result$mad_value >= 0)
  
  # Since data was sampled from Benford distribution, MAD should be low
  expect_lt(result$mad_value, 0.02)
  
  # Comparison table should have 9 rows
  expect_equal(nrow(result$comparison_table), 9)
})

test_that("benford_mad_test detects non-Benford distribution (uniform)", {
  set.seed(123)
  uniform_digits <- sample(1:9, size = 1000, replace = TRUE, prob = rep(1/9, 9))
  
  result <- benford_mad_test(uniform_digits, mode = 1)
  
  expect_true(result$mad_value > 0.02)
})

test_that("benford_mad_test handles first two digits correctly", {
  # Generate first two digits from Benford's distribution for first two digits
  benford_probs_two <- generate_benford_distribution_first_two_digits()
  digits_10_99 <- as.numeric(names(benford_probs_two))
  sampled_digits <- sample(digits_10_99, size = 1000, replace = TRUE, prob = benford_probs_two)
  
  result <- benford_mad_test(sampled_digits, mode = 3)
  
  expect_type(result, "list")
  expect_true(result$mad_value >= 0)
  expect_equal(nrow(result$comparison_table), 90)
})

test_that("benford_mad_test returns NULL for empty input", {
  expect_warning(
    res <- benford_mad_test(numeric(0), mode = 1),
    regexp = "MAD test cannot be performed"
  )
  expect_null(res)
})

test_that("benford_mad_test errors on invalid inputs", {
  expect_error(benford_mad_test("not numeric", mode = 1), "must be a numeric vector")
  expect_error(benford_mad_test(1:9, mode = 99), "must be 1")
})

library(ggplot2)

# Dummy expected distributions
generate_benford_distribution <- function(mode) {
  if (mode == 1) {
    p <- log10(1 + 1 / (1:9))
    names(p) <- as.character(1:9)
    return(p)
  } else if (mode == 2) {
    p <- rep(0.1, 10)
    names(p) <- as.character(0:9)
    return(p)
  }
}

generate_benford_distribution_first_two_digits <- function() {
  digits <- 10:99
  p <- log10(1 + 1 / digits)
  names(p) <- as.character(digits)
  return(p)
}

# Test suite
test_that("plot_benford_distribution works for first digit mode", {
  obs <- sample(1:9, 100, replace = TRUE)
  p <- plot_benford_distribution(obs, mode = 1, plot_title = "Test First Digit")
  expect_s3_class(p, "ggplot")
})

test_that("plot_benford_distribution works for second digit mode", {
  obs <- sample(0:9, 100, replace = TRUE)
  p <- plot_benford_distribution(obs, mode = 2, plot_title = "Test Second Digit")
  expect_s3_class(p, "ggplot")
})

test_that("plot_benford_distribution works for first two digits mode", {
  obs <- sample(10:99, 200, replace = TRUE)
  p <- plot_benford_distribution(obs, mode = 3, plot_title = "Test First Two Digits")
  expect_s3_class(p, "ggplot")
})

test_that("plot_benford_distribution returns NULL with empty observed_digits", {
  expect_warning({
    p <- plot_benford_distribution(numeric(0), mode = 1)
    expect_null(p)
  }, "No observed digits provided")
})

test_that("plot_benford_distribution errors with non-numeric observed_digits", {
  expect_error(plot_benford_distribution(c("a", "b", "c"), mode = 1),
               "must be a numeric vector")
})

test_that("plot_benford_distribution errors with invalid mode", {
  expect_error(plot_benford_distribution(1:9, mode = 4),
               "must be 1 \\(first digit\\), 2 \\(second digit\\), or 3 \\(first two digits\\)")
})

