#' Calculate Median Symmetric Accuracy
#'
#' This function calculates the median symmetric accuracy, a robust metric for model
#' performance as described in Morley et al. (2018), "Measures of model performance
#' based on the log accuracy ratio," Space Weather, 16, 8–21.
#'
#' The formula is: 100 * (exp(median(abs(log(predicted/observed)))) - 1)
#'
#' @param predicted A numeric vector of predicted (or modeled) values.
#' @param observed A numeric vector of observed (or actual) values.
#' @param na.rm A logical value indicating whether NA values should be
#'   stripped before the computation proceeds. Defaults to TRUE.
#'
#' @return A single numeric value representing the median symmetric accuracy
#'   as a percentage.
#'
#' @examples
#' # Example from space physics where values can span orders of magnitude
#' observed_flux <- c(1.2e3, 4.5e4, 8.0e2, 1.5e5, 9.9e3)
#' predicted_flux <- c(1.4e3, 3.9e4, 9.1e2, 1.2e5, 1.1e4)
#'
#' accuracy <- median_symmetric_accuracy(predicted_flux, observed_flux)
#' print(paste0("Median Symmetric Accuracy: ", round(accuracy, 2), "%"))
#'
calc_msa <- function(predicted, observed, na.rm = TRUE) {
  
  # --- Input Validation ---
  
  # Check if inputs are numeric
  if (!is.numeric(predicted) || !is.numeric(observed)) {
    stop("Both 'predicted' and 'observed' must be numeric vectors.")
  }
  
  # Check if lengths are equal
  if (length(predicted) != length(observed)) {
    stop("'predicted' and 'observed' vectors must have the same length.")
  }
  
  # Remove NA values if na.rm is TRUE
  if (na.rm) {
    complete_cases <- complete.cases(predicted, observed)
    predicted <- predicted[complete_cases]
    observed <- observed[complete_cases]
  }
  
  # Check for non-positive values, which are invalid for log ratio
  if (any(predicted <= 0) || any(observed <= 0)) {
    stop("All values in 'predicted' and 'observed' must be positive for this metric.")
  }
  
  # --- Calculation ---
  
  # 1. Calculate the accuracy ratio
  accuracy_ratio <- predicted / observed
  
  # 2. Calculate the natural logarithm of the ratio
  log_accuracy_ratio <- log(accuracy_ratio)
  
  # 3. Take the absolute value
  abs_log_accuracy_ratio <- abs(log_accuracy_ratio)
  
  # 4. Find the median of these values
  median_val <- median(abs_log_accuracy_ratio, na.rm = na.rm) # na.rm is technically redundant here
  # due to earlier cleaning
  
  # 5. Convert back to a percentage
  # This represents the typical percentage error.
  msa_percent <- 100 * (exp(median_val) - 1)
  
  return(msa_percent)
}

calc_msa_boot <- function(dat, indices) {
  calc_msa(dat$predicted[indices], dat$observed[indices])
}

