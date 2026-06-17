plot_obsfit <- function(mod1, dat1, mod2 = NULL, dat2 = NULL, date_range = NULL){
  
  # Ensure output directory exists
  plot_dir <- "./plots"
  if (!dir.exists(plot_dir)) dir.create(plot_dir)
  
  # ==============================================================================
  # --- HELPER FUNCTION: Process a single model and dataset ---
  # ==============================================================================
  process_dat <- function(mod, dat) {
    dat_with_preds <- dat %>%
      ungroup() %>%
      mutate(
        W_i_det = exp(mod$rep$log_W_i_det),
        data_source = case_when(
          data_source == "NMFS BTS" ~ "Survey",
          data_source == "Port"     ~ "Port aggregate",
          data_source == "Port DMF" ~ "Port individual",
          TRUE                      ~ data_source 
        ),
        stock_spp_label = paste(stock_label, species_label, sep = " ")
      )
    
    if(!"DATE" %in% names(dat_with_preds)){
      dat_with_preds <- dat_with_preds %>% 
        mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
    }
    
    dat_with_preds <- dat_with_preds %>%
      mutate(DECADE = paste0((YEAR %/% 10) * 10, "s"))
    
    if(!is.null(date_range)) {
      dat_with_preds <- dat_with_preds %>% 
        filter(DATE >= as.Date(date_range[1]) & DATE <= as.Date(date_range[2]))
    }
    
    if(nrow(dat_with_preds) == 0) return(NULL)
    
    # Aggregate & Define Quintiles
    dat_wfit <- dat_with_preds %>%
      group_by(stock_spp_label, data_source, stock_label, species_label, DECADE, YEAR, MONTH, WEEK, LOC, s, Wobs_s) %>%
      summarise(Wpred_s = sum(W_i_det), .groups = "drop") %>%
      group_by(data_source) %>% 
      mutate(
        Wbin = cut(Wobs_s, 
                   breaks = quantile(Wobs_s, probs = seq(0, 1, .2), na.rm = TRUE) - 
                     c(0.0001, rep(0, 5)),
                   include.lowest = TRUE)
      )
    
    # Label Cleaning
    lvl <- levels(dat_wfit$Wbin)
    clean_lvl <- gsub("\\(|\\[|\\)|\\]", "", lvl)
    new_labels <- sapply(strsplit(clean_lvl, ","), function(x) {
      low <- round(as.numeric(x[1]), 1)
      high <- round(as.numeric(x[2]), 1)
      paste0(low, " - ", high, " kg")
    })
    levels(dat_wfit$Wbin) <- new_labels
    
    dat_wfit <- dat_wfit %>%
      mutate(
        log_ratio = log(Wpred_s) - log(Wobs_s),
        Bias_pct = sign(log_ratio) * 100 * (exp(abs(log_ratio)) - 1),
        Error_pct = 100 * (exp(abs(log_ratio)) - 1)
      )
    
    # Calculate Metrics
    stats_summary <- dat_wfit %>%
      group_by(stock_spp_label, data_source) %>%
      summarise(
        R2 = 1 - (sum((Wobs_s - Wpred_s)^2, na.rm = TRUE) / 
                    sum((Wobs_s - mean(Wobs_s, na.rm = TRUE))^2, na.rm = TRUE)),
        min_obs = min(Wobs_s, na.rm = TRUE),
        max_pred = max(Wpred_s, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(R2_label = paste0("R^2 == ", round(R2, 3)))
    
    bias_by_bin <- dat_wfit %>%
      group_by(stock_spp_label, data_source, Wbin) %>%
      summarise(
        Median_Bias = median(Bias_pct, na.rm = TRUE),
        Median_Err  = median(Error_pct, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      mutate(Label = sprintf("Median Bias: %.1f%% | Median Error: %.1f%%", Median_Bias, Median_Err))
    
    return(list(dat_wfit = dat_wfit, stats_summary = stats_summary, bias_by_bin = bias_by_bin))
  }
  
  
  # ==============================================================================
  # --- PROCESS DATA ---
  # ==============================================================================
  prep1 <- process_dat(mod1, dat1)
  if(is.null(prep1)) stop("No data found for Model 1 within the specified date range.")
  
  models_to_run <- list(prep1)
  dat_wfit_combined <- prep1$dat_wfit
  stats_combined    <- prep1$stats_summary
  
  # Process second model if provided
  if (!is.null(mod2) && !is.null(dat2)) {
    prep2 <- process_dat(mod2, dat2)
    if(!is.null(prep2)) {
      models_to_run[[2]] <- prep2
      dat_wfit_combined <- dplyr::bind_rows(dat_wfit_combined, prep2$dat_wfit)
      stats_combined    <- dplyr::bind_rows(stats_combined, prep2$stats_summary)
    }
  }
  
  # ==============================================================================
  # --- PLOT 1: Combined 1-to-1 Observed vs. Predicted ---
  # ==============================================================================
  # Build dynamic subtitle for the combined plot
  subtitle_info <- dat_wfit_combined %>%
    group_by(stock_spp_label) %>%
    summarise(
      min_yr = min(YEAR, na.rm = TRUE),
      max_yr = max(YEAR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label_with_years = paste0(stock_spp_label, " (", min_yr, "-", max_yr, ")"))
  
  single_input <- length(models_to_run) == 1
  sub_text <- if(single_input) paste(subtitle_info$label_with_years, collapse = " & ") else NULL
  
  file_tag <- paste(gsub(" ", "_", unique(dat_wfit_combined$stock_spp_label)), collapse = "_and_")
  date_label <- paste(range(dat_wfit_combined$YEAR, na.rm = TRUE), collapse = "_")
  
  p_obs_pred_faceted <- ggplot(dat_wfit_combined, aes(x = Wobs_s, y = Wpred_s)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_point(alpha = 0.4) +
    scale_x_log10() + 
    scale_y_log10() +
    theme_bw() +
    labs(title = "Observed vs. Fit Weight", 
         subtitle = sub_text,
         x = "Observed Sample Weight (kg)", 
         y = "Fit Sample Weight (kg)") +
    geom_label(data = stats_combined, 
               aes(x = min_obs, y = max_pred, label = R2_label),
               parse = TRUE, hjust = -0.1, vjust = 1.2, fill = "white",
               inherit.aes = FALSE) +
    ggh4x::facet_grid2(data_source ~ stock_spp_label, scales = "free", independent = "all")
  
  print(p_obs_pred_faceted)
  combined_path <- sprintf("%s/obsfit_1to1_FACETED_%s_%s.jpg", plot_dir, file_tag, date_label)
  ggsave(combined_path, plot = p_obs_pred_faceted, width = 12, height = 8, dpi = 300)
  message("Saved combined 1-to-1 plot to:\n  - ", combined_path)
  
  
  # ==============================================================================
  # --- PLOTS 2 & 3: Plotting Loop for Residual/Bias Plots ---
  # ==============================================================================
  # We loop over each provided model/dataset independently to prevent massive grids
  for (prep_data in models_to_run) {
    
    loop_dat_wfit <- prep_data$dat_wfit
    loop_bias     <- prep_data$bias_by_bin
    
    stk_label <- unique(loop_dat_wfit$stock_spp_label)[1]
    stk_tag   <- gsub(" ", "_", stk_label)
    
    # Loop over individual data sources exactly as requested
    for(src in unique(loop_dat_wfit$data_source)) {
      
      sub_dat  <- loop_dat_wfit %>% filter(data_source == src)
      sub_bias <- loop_bias %>% filter(data_source == src)
      
      if(nrow(sub_dat) == 0) next
      
      sub_title <- paste0("Source: ", src, " | ", stk_label)
      
      # --- PLOT 2: Overall Bias Summary ---
      p_res_summary <- ggplot(sub_dat, aes(x = Wobs_s, y = Bias_pct)) +
        geom_point(alpha = 0.4, color = "grey30") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_text(data = sub_bias, aes(x = -Inf, y = Inf, label = Label),
                  hjust = -0.05, vjust = 1.5, size = 3.5, fontface = "bold", inherit.aes = FALSE) +
        ggh4x::facet_grid2(Wbin ~ ., scales = "free_x", independent = "x") + 
        theme_bw() +
        labs(title = "Overall Model Bias & Error by Weight Quintile", subtitle = sub_title,
             x = "Observed Sample Weight (kg)", y = "Bias (%)")
      
      # --- PLOT 3: Bias Faceted by DECADE ---
      p_res_decade <- ggplot(sub_dat, aes(x = Wobs_s, y = Bias_pct)) +
        geom_point(alpha = 0.3, color = "grey30") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        ggh4x::facet_grid2(Wbin ~ DECADE, scales = "free_x", independent = "x") +
        theme_bw() +
        labs(title = "Decadal Model Bias by Weight Quintile", subtitle = sub_title,
             x = "Observed Sample Weight (kg)", y = "Bias (%)")
      
      # Print and save
      print(p_res_summary)
      print(p_res_decade)
      
      source_tag <- gsub(" ", "_", src)
      
      paths <- c(
        sprintf("%s/obsfit_BIAS_SUMMARY_%s_%s_%s.jpg", plot_dir, stk_tag, source_tag, date_label),
        sprintf("%s/obsfit_BIAS_DECADAL_%s_%s_%s.jpg", plot_dir, stk_tag, source_tag, date_label)
      )
      
      ggsave(paths[1], plot = p_res_summary, width = 8, height = 10, dpi = 300)
      ggsave(paths[2], plot = p_res_decade, width = 12, height = 10, dpi = 300)
      
      message("Saved bias plots for '", stk_label, "' - Source '", src, "' to:")
      lapply(paths, function(x) message("  - ", x))
    }
  }
}