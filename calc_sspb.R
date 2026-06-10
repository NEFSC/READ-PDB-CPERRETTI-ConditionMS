#' Calculate Symmetric Signed Percentage Bias
#'
#' This function calculates the symmetric signed percentage bias, a robust metric for model
#' bias as described in Morley et al. (2018). The sign of the result indicates the 
#' direction of the bias (positive for overprediction, negative for underprediction).
#'
#' @param predicted A numeric vector of predicted (or modeled) values.
#' @param observed A numeric vector of observed (or actual) values.
#' @param na.rm A logical value indicating whether NA values should be
#'   stripped before the computation proceeds. Defaults to TRUE.
#'
#' @return A single numeric value representing the symmetric signed percentage bias.
#'
calc_sspb <- function(predicted, observed, na.rm = TRUE) {
  
  # --- Input Validation ---
  if (!is.numeric(predicted) || !is.numeric(observed)) {
    stop("Both 'predicted' and 'observed' must be numeric vectors.")
  }
  if (length(predicted) != length(observed)) {
    stop("'predicted' and 'observed' vectors must have the same length.")
  }
  if (na.rm) {
    complete_cases <- complete.cases(predicted, observed)
    predicted <- predicted[complete_cases]
    observed <- observed[complete_cases]
  }
  if (any(predicted <= 0) || any(observed <= 0)) {
    stop("All values in 'predicted' and 'observed' must be positive for this metric.")
  }
  
  # --- Calculation ---
  
  # 1. Calculate the log of the accuracy ratio
  log_accuracy_ratio <- log(predicted / observed)
  
  # 2. Find the median of the log accuracy ratio. This value's sign indicates bias direction.
  median_log_ratio <- median(log_accuracy_ratio, na.rm = na.rm)
  
  # 3. Calculate the magnitude of the bias
  bias_magnitude <- exp(abs(median_log_ratio)) - 1
  
  # 4. Get the sign of the bias
  bias_sign <- sign(median_log_ratio)
  
  # 5. Combine and convert to percentage
  sspb_percent <- 100 * bias_sign * bias_magnitude
  
  return(sspb_percent)
}

calc_sspb_boot <- function(dat, indices) {
  calc_sspb(dat$predicted[indices], dat$observed[indices])
}