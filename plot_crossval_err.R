plot_crossval_err <- function(crossval_out, dat, models_to_plot = NULL) {
  
  # Extract both data frames
  crossval_results <- crossval_out$crossval_results
  crossval_err <- crossval_out$crossval_err
  
  # --- Step 0.5: Subset Models by Base Name if Requested ---
  if (!is.null(models_to_plot)) {
    
    # 1. Filter crossval_results first
    if ("model_name" %in% colnames(crossval_results)) {
      crossval_results <- crossval_results %>% filter(model_name %in% models_to_plot)
    } else {
      crossval_results <- crossval_results %>% 
        filter(trimws(gsub(":.*", "", name_and_effects)) %in% models_to_plot)
    }
    
    # 2. Extract the exact 'name_and_effects' strings that survived
    surviving_models <- unique(crossval_results$name_and_effects)
    
    # 3. Filter crossval_err using those surviving strings
    crossval_err <- crossval_err %>% filter(name_and_effects %in% surviving_models)
    
    if (nrow(crossval_results) == 0 || nrow(crossval_err) == 0) {
      stop("No models matched the names provided in 'models_to_plot'. Check your spelling.")
    }
  }
  
  # --- Step 0: Standardize data source names ---
  crossval_err <- crossval_err %>%
    mutate(
      data_source = if("data_source" %in% names(.)) {
        case_when(
          data_source == "NMFS BTS" ~ "Survey",
          data_source == "Port"     ~ "Port aggregate",
          data_source == "Port DMF" ~ "Port individual",
          TRUE                      ~ data_source
        )
      } else { NULL }
    )
  
  # We find the unique values in the order they appear in the results table
  model_order <- unique(crossval_results$name_and_effects)
  
  # Apply those levels as a factor to the error table
  crossval_err <- crossval_err %>%
    mutate(name_and_effects = factor(name_and_effects, levels = model_order))
  
  # --- Step 1: Check for conflicting columns ---
  has_data_source <- "data_source" %in% colnames(crossval_err)
  has_fold <- "fold" %in% colnames(crossval_err)
  
  if (has_data_source && has_fold) {
    stop("The data frame cannot contain both 'data_source' and 'fold' columns for plotting.")
  }
  
  # --- Step 2: Set up color variable and renaming ---
  color_var <- NULL
  
  if (has_data_source) {
    crossval_err <- crossval_err %>% rename(`Data source` = data_source)
    color_var <- "Data source"
  } else if (has_fold) {
    crossval_err <- crossval_err %>% mutate(Fold = as.factor(fold))
    color_var <- "Fold"
  }
  
  # --- Step 3: Combine Accuracy and Bias into a single long dataframe ---
  df_acc <- crossval_err %>%
    mutate(
      Metric = "Absolute error (%)",
      Estimate = msa,
      lo = msa_90lo,
      hi = msa_90hi
    )
  
  df_bia <- crossval_err %>%
    mutate(
      Metric = "Bias (%)",
      Estimate = sspb,
      lo = sspb_90lo,
      hi = sspb_90hi
    )
  
  df_combined <- bind_rows(df_acc, df_bia) %>%
    mutate(Metric = factor(Metric, levels = c("Absolute error (%)", "Bias (%)")))
  
  # ============================================================================
  # 1. Plot Combined Error & Bias
  # ============================================================================
  p_combined <- ggplot(df_combined, aes(x = name_and_effects, y = Estimate))
  
  # Group dynamically inside the aes() data mask
  if (!is.null(color_var)) {
    p_combined <- p_combined + aes(color = .data[[color_var]], group = .data[[color_var]])
  } else {
    p_combined <- p_combined + aes(group = 1)
  }
  
  # Create a dummy dataframe for the horizontal line so it only plots on the Bias facet
  ref_line <- data.frame(
    Metric = factor("Bias (%)", levels = levels(df_combined$Metric)), 
    yval = 0
  )
  
  p_final <-
    p_combined + 
    geom_hline(data = ref_line, aes(yintercept = yval), linewidth = 0.1, color = "black") +
    geom_point() +
    geom_line() + 
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.0, alpha = 0.7) +
    facet_wrap(~Metric, scales = "free_y", ncol = 1) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold", size = 11)
    ) +
    ggtitle(ifelse(grepl("Time series", unique(crossval_err$test_type)),
                   "Time series cross-validation error and bias",
                   paste0(unique(crossval_err$test_type), " error and bias")),
            subtitle = paste0(unique(dat$stock_label), " ",
                              unique(dat$species_label), " (",
                              min(dat$YEAR), "-", 
                              max(dat$YEAR), ")")) +
    ylab("") + 
    xlab("Model") + 
    labs(caption = "Positive bias indicates over-prediction.")
  
  print(p_final)
  
  # Save the combined plot
  if(!dir.exists("./plots")) dir.create("./plots")
  filename_comb <- paste0("./plots/", 
                          ifelse(grepl("Time series", unique(crossval_err$test_type)),
                                 "Time series cross-validation error_bias_",
                                 paste0(unique(crossval_err$test_type), " error_bias_")),
                          paste0(unique(dat$stock_label), "_",
                                 unique(dat$species_label), "_(",
                                 min(dat$YEAR), "-", 
                                 max(dat$YEAR), ")"),
                          ".jpg")
  ggsave(plot = p_final, filename_comb, w = 9, h = 10, dpi = 300)
  message("Plot saved as ", filename_comb)
  
  # ============================================================================
  # 2. Time-Series-Specific Error Metric Forecast Horizons
  # ============================================================================
  if(grepl("Time series", unique(crossval_err$test_type))) {
    
    # Standardize raw results formatting
    crossval_res_clean <- crossval_results %>%
      mutate(
        data_source = if("data_source" %in% names(.)) {
          case_when(
            data_source == "NMFS BTS" ~ "Survey",
            data_source == "Port"     ~ "Port aggregate",
            data_source == "Port DMF" ~ "Port individual",
            TRUE                      ~ data_source
          )
        } else { NULL },
        name_and_effects = factor(name_and_effects, levels = model_order)
      )
    
    if (has_data_source) {
      crossval_res_clean <- crossval_res_clean %>% rename(`Data source` = data_source)
    } else if (has_fold) {
      crossval_res_clean <- crossval_res_clean %>% mutate(Fold = as.factor(fold))
    }
    
    # Calculate years since peel start
    grouping_cols <- c("name_and_effects", "fold")
    crossval_results_tsmsa <- crossval_res_clean %>%
      group_by(across(all_of(grouping_cols))) %>%
      mutate(peel_start = as.character(min(lubridate::year(YW))),
             years_since_peel_start = round(as.numeric((YW - min(YW)))/365))
    
    # Bootstrap MSA by forecast horizon
    reframe_cols <- c("name_and_effects", "years_since_peel_start")
    if (!is.null(color_var)) reframe_cols <- c(reframe_cols, color_var)
    
    crossval_results_tsmsa <- crossval_results_tsmsa %>%
      group_by(across(all_of(reframe_cols))) %>%
      reframe(
        msa = calc_msa(pred_Wobs_s_test, Wobs_s),
        msa_90ci = quantile(boot::boot(dat = data.frame(predicted = pred_Wobs_s_test,
                                                        observed  = Wobs_s),
                                       statistic = calc_msa_boot, R = 1000)$t,
                            probs = c(0.05, 0.95))
      ) %>%
      group_by(across(all_of(reframe_cols))) %>%
      mutate(msa_90lo = min(msa_90ci),
             msa_90hi = max(msa_90ci)) %>%
      distinct(across(-c(msa_90ci)))
    
    # Build Forecast Horizon Plot
    p_tsmsa <-
      ggplot(crossval_results_tsmsa, aes(x = years_since_peel_start, y = msa)) +
      geom_line(aes(color = name_and_effects)) +
      geom_ribbon(aes(ymin = msa_90lo, ymax = msa_90hi, fill = name_and_effects), alpha = 0.2) +
      theme_bw() +
      ggtitle("Weight prediction error with increasing forecast horizon",
              subtitle = paste0(unique(dat$stock_label), " ", unique(dat$species_label))) +
      labs(caption = "Forecast horizon is rounded to the nearest year.\nThe line is the median error; the shaded interval is the 90% confidence interval.",
           color = "Model", fill = "Model") +
      xlab("Forecast horizon (years)") +
      ylab("Absolute error (%)")
    
    # Drop model legend if only one model is plotted
    if (length(unique(crossval_results_tsmsa$name_and_effects)) == 1) {
      p_tsmsa <- p_tsmsa + theme(legend.position = "none")
    }
    
    # Facet dynamically if there are multiple data sources or folds
    if (!is.null(color_var)) {
      p_tsmsa <- p_tsmsa + facet_wrap(as.formula(paste0("~`", color_var, "`")), scales = "free_y", ncol = 1)
    }
    
    print(p_tsmsa)
    
    # Save Forecast Horizon Plot
    stock_lbl   <- gsub(" ", "_", unique(dat$stock_label))
    species_lbl <- gsub(" ", "_", unique(dat$species_label))
    filename_tsmsa <- paste0("./plots/crossval_forecast_horizon_error_", stock_lbl, "_", species_lbl, ".jpg")
    ggsave(plot = p_tsmsa, filename = filename_tsmsa, width = 11, height = 9, dpi = 300)
    message("Plot saved as ", filename_tsmsa)
  }
}