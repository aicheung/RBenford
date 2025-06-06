# Ensure you have ggplot2 installed for enhanced plotting capabilities
# install.packages("ggplot2")
library(ggplot2)

#' Plots the observed digit distribution against the theoretical Benford distribution.
#'
#' This function generates a bar plot comparing the actual frequencies of leading
#' digits from your dataset with the expected frequencies according to Benford's Law.
#' It supports plotting for first digits, second digits, or first two digits.
#'
#' @param observed_digits A numeric vector of the extracted leading digits
#'                        (e.g., first digits, second digits, or first two digits combined).
#' @param mode An integer specifying which distribution to plot:
#'   1: First digit (digits 1-9).
#'   2: Second digit (digits 0-9).
#'   3: First two digits (digits 10-99).
#' @param plot_title A string to use as the title of the plot.
#' @param x_axis_label A string for the x-axis label.
#' @param y_axis_label A string for the y-axis label.
#' @return A ggplot object, which can be printed to display the plot.
#'         Returns NULL if there are insufficient data points after filtering.
#' @examples
#' # (Assuming clean_benford_data, extract_leading_digits,
#' # generate_benford_distribution, generate_benford_distribution_first_two_digits are sourced)
#'
#' # Create dummy data
#' set.seed(456)
#' data_example <- c(sample(1:9, 500, replace = TRUE, prob = generate_benford_distribution(1)),
#'                   sample(10:99, 100, replace = TRUE)) # Mix in some values
#' cleaned_data_plot <- data_example[data_example > 0]
#'
#' # Example 1: Plotting First Digits
#' first_digits_plot <- extract_leading_digits(cleaned_data_plot, mode = 1)
#' if (length(first_digits_plot) > 0) {
#'   p1 <- plot_benford_distribution(first_digits_plot, mode = 1,
#'                                   plot_title = "First Digit Distribution Comparison")
#'   print(p1)
#' }
#'
#' # Example 2: Plotting Second Digits
#' second_digits_plot <- extract_leading_digits(cleaned_data_plot, mode = 2)
#' if (length(second_digits_plot) > 0) {
#'   p2 <- plot_benford_distribution(second_digits_plot, mode = 2,
#'                                   plot_title = "Second Digit Distribution Comparison")
#'   print(p2)
#' }
#'
#' # Example 3: Plotting First Two Digits
#' two_digits_list_plot <- extract_leading_digits(cleaned_data_plot, mode = 3)
#' valid_indices_plot <- !is.na(two_digits_list_plot$first_digits) & !is.na(two_digits_list_plot$second_digits)
#' first_two_digits_plot <- two_digits_list_plot$first_digits[valid_indices_plot] * 10 +
#'                          two_digits_list_plot$second_digits[valid_indices_plot]
#'
#' if (length(first_two_digits_plot) > 0) {
#'   p3 <- plot_benford_distribution(first_two_digits_plot, mode = 3,
#'                                   plot_title = "First Two Digits Distribution Comparison",
#'                                   x_axis_label = "First Two Digits (10-99)")
#'   print(p3)
#' }
plot_benford_distribution <- function(observed_digits, mode,
                                      plot_title = "Digit Distribution Comparison",
                                      x_axis_label = "Digit",
                                      y_axis_label = "Frequency") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for this function. Please install it with install.packages('ggplot2')")
  }
  
  if (!is.numeric(observed_digits)) {
    stop("Error: 'observed_digits' must be a numeric vector.")
  }
  if (!mode %in% c(1, 2, 3)) {
    stop("Error: 'mode' must be 1 (first digit), 2 (second digit), or 3 (first two digits).")
  }
  
  n_observations <- length(observed_digits)
  if (n_observations == 0) {
    warning("No observed digits provided. Plot cannot be generated.")
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
  
  # 2. Calculate observed frequencies (proportions)
  observed_counts_table <- table(factor(observed_digits, levels = all_possible_digits))
  observed_frequencies <- as.numeric(observed_counts_table) / n_observations
  names(observed_frequencies) <- names(observed_counts_table)
  
  # Ensure observed frequencies align with expected probabilities by their names
  observed_frequencies <- observed_frequencies[names(benford_expected_probs)]
  
  # 3. Prepare data for ggplot2
  # Combine observed and expected frequencies into a single data frame
  plot_data <- data.frame(
    Digit = factor(names(benford_expected_probs), levels = names(benford_expected_probs)), # Ensure order
    Frequency = c(observed_frequencies, benford_expected_probs),
    Type = factor(c(rep("Observed", length(observed_frequencies)),
                    rep("Benford Expected", length(benford_expected_probs))),
                  levels = c("Observed", "Benford Expected")) # Define factor levels for legend order
  )
  
  # Adjust x-axis labels for mode 3 if there are too many
  if (mode == 3 && length(all_possible_digits) > 20) { # Arbitrary cutoff, adjust as needed
    x_breaks <- all_possible_digits[seq(1, length(all_possible_digits), by = 5)] # Show every 5th label
    x_labels <- all_possible_digits[seq(1, length(all_possible_digits), by = 5)]
  } else {
    x_breaks <- all_possible_digits
    x_labels <- all_possible_digits
  }
  
  
  # 4. Create the plot using ggplot2
  p <- ggplot(plot_data, aes(x = Digit, y = Frequency, fill = Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("Observed" = "steelblue", "Benford Expected" = "darkorange")) +
    labs(title = plot_title,
         x = x_axis_label,
         y = y_axis_label,
         fill = "Distribution Type") + # Label for the legend
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
    scale_x_discrete(breaks = x_breaks, labels = x_labels) # Apply custom breaks for mode 3
  
  # Add text labels for frequencies (optional, but good for exact values)
  # p <- p + geom_text(aes(label = round(Frequency, 3)),
  #                    position = position_dodge(width = 0.8),
  #                    vjust = -0.5, size = 3)
  
  return(p)
}