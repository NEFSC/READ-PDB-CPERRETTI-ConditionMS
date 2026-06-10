library(dplyr)
library(ggplot2)

plot_combined_crossval_err <- function(cv_list, dat_info = dat) {
  
  # 1. Establish model order from the first object's results table
  # (Assuming all objects in the list contain the same models)
  model_order <- unique(cv_list[[1]]$crossval_results$name_and_effects)
  
  # 2. Combine all error data frames
  combined_err <- lapply(cv_list, function(x) {
    return(x$crossval_err)
  }) %>%
    bind_rows()
  
  # 3. Apply the factor levels to ensure correct x-axis order
  combined_err <- combined_err %>%
    mutate(name_and_effects = factor(name_and_effects, levels = model_order))
  
  # 4. Metadata for titles
  subtitle_text <- paste0(unique(dat_info$stock_label), " ",
                          unique(dat_info$species_label), " (",
                          min(dat_info$YEAR), "-", 
                          max(dat_info$YEAR), ")")
  
  # 5. Use position_dodge so different techniques don't overlap
  pd <- position_dodge(width = 0.6)
  
  # --- Accuracy Plot ---
  p_acc <- ggplot(combined_err, 
                  aes(x = name_and_effects, y = msa, 
                      color = test_type, group = test_type)) +
    geom_point(position = pd, size = 2) +
    geom_line(position = pd, alpha = 0.4) +
    geom_errorbar(aes(ymin = msa_90lo, ymax = msa_90hi), 
                  width = 0.1, alpha = 0.7, position = pd) +
    theme_bw() +
    labs(title = "Cross-validation Accuracy Comparison",
         subtitle = subtitle_text,
         y = "Absolute error (%)",
         x = "Model",
         color = "CV Technique") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- Bias Plot ---
  p_bia <- ggplot(combined_err, 
                  aes(x = name_and_effects, y = sspb, 
                      color = test_type, group = test_type)) +
    geom_point(position = pd, size = 2) +
    geom_line(position = pd, alpha = 0.4) +
    geom_errorbar(aes(ymin = sspb_90lo, ymax = sspb_90hi), 
                  width = 0.1, alpha = 0.7, position = pd) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    labs(title = "Cross-Validation Bias Comparison",
         subtitle = subtitle_text,
         y = "Bias (%)",
         x = "Model",
         color = "CV Technique",
         caption = "Positive bias indicates over-prediction.") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_acc)
  print(p_bia)
}