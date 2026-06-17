plot_crossval_obspred <- function(crossval_results1, crossval_results2 = NULL) {
  
  # --- 0. Setup Directory, Standardization, and Combination ---
  if(!dir.exists("./plots")) dir.create("./plots")
  
  # Helper function to standardize a single crossval_results dataframe
  process_cv <- function(df) {
    df %>%
      rename(Model = name_and_effects) %>%
      mutate(data_source = case_when(
        data_source == "NMFS BTS" ~ "Survey",
        data_source == "Port"     ~ "Port aggregate",
        data_source == "Port DMF" ~ "Port individual",
        TRUE                      ~ data_source
      ))
  }
  
  # Standardize the first dataframe
  crossval_results1 <- process_cv(crossval_results1)
  
  # Combine with second dataframe if provided
  if (!is.null(crossval_results2)) {
    crossval_results2 <- process_cv(crossval_results2)
    crossval_results <- dplyr::bind_rows(crossval_results1, crossval_results2)
  } else {
    crossval_results <- crossval_results1
  }
  
  # Create a combined Stock & Species label for faceting
  crossval_results <- crossval_results %>%
    mutate(stock_spp_label = paste(stock_label, species_label, sep = " "))
  
  # Clean up strings for safe file paths
  file_tag <- paste(gsub(" ", "_", unique(crossval_results$stock_spp_label)), collapse = "_and_")
  test_lbl <- ifelse(grepl("Time series", unique(crossval_results$test_type)[1]), "tscv", "cv")
  
  # Dynamically extract global metadata for subtitles
  subtitle_info <- crossval_results %>%
    group_by(stock_spp_label) %>%
    summarise(
      min_yr = min(lubridate::year(YW), na.rm = TRUE),
      max_yr = max(lubridate::year(YW), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label_with_years = paste0(stock_spp_label, " (", min_yr, "-", max_yr, ")"))
  
  sub_text <- paste(subtitle_info$label_with_years, collapse = " & ")
  
  crossval_err_s <-
    crossval_results %>%
    distinct(stock_spp_label, species_label, stock_label, test_type, Model, fold, nlminb_converged,
             data_source, s, YW, Wobs_s, pred_Wobs_s_test)
  
  # Determine if we only have a single model
  single_model <- length(unique(crossval_err_s$Model)) == 1
  single_model_name <- unique(crossval_err_s$Model)[1]
  
  # ============================================================================
  # 1. Plot percent error over time
  # ============================================================================
  p_et <-
    ggplot(crossval_err_s, aes(x = YW, y = 100 * (pred_Wobs_s_test/Wobs_s - 1 ))) +
    geom_point(alpha = 0.5, aes(color = 100 * (pred_Wobs_s_test/Wobs_s - 1 ))) +
    geom_hline(yintercept = 0, linetype = 2) +
    
    # Solid line showing the median percent error BY YEAR
    stat_summary(
      aes(x = as.Date(paste0(lubridate::year(YW), "-07-01")), group = 1),
      fun = median, 
      geom = "line", 
      linetype = "solid", 
      color = "black", 
      linewidth = 0.8
    ) +
    
    ylab("Error (%)") +
    theme_bw() +
    labs(caption = "Positive errors indicate over-prediction. Black line shows yearly median error trend.") +
    xlab("Date") +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "Time series cross-validation error over time",
                   paste0(unique(crossval_results$test_type)[1], " error over time")),
            subtitle = sub_text) +
    # Facet grid to handle multiple data sources, stocks, and models
    ggh4x::facet_grid2(data_source ~ stock_spp_label + Model, independent = "all", scales = "free") +
    theme(legend.title = element_blank()) +
    scale_color_gradient2(
      low = "darkred", 
      mid = "gray95", 
      high = "darkblue", 
      midpoint = 0, 
      trans = "pseudo_log"
    )
  
  print(p_et)
  filename_et <- paste0("./plots/crossval_error_time_", test_lbl, "_", file_tag, ".jpg")
  ggsave(plot = p_et, filename = filename_et, width = 16, height = 10, dpi = 300)
  
  # ============================================================================
  # 1.1 Pre-calculate and plot 5-Year Static Block Median Symmetric Accuracy (MSA)
  # ============================================================================
  msa_blocks <- crossval_err_s %>%
    mutate(
      Year = lubridate::year(YW),
      # Create explicit non-overlapping 5-year blocks (e.g., 1980, 1985, 1990)
      Block_Start = (Year %/% 5) * 5
    ) %>%
    group_by(stock_spp_label, data_source, Model, Block_Start) %>%
    summarise(
      msa_value = calc_msa(predicted = pred_Wobs_s_test, observed = Wobs_s),
      .groups = "drop"
    ) %>%
    mutate(
      Block_Mid_Date = as.Date(paste0(Block_Start + 2, "-07-01"))
    )
  
  p_msa_5yr_block <-
    ggplot(msa_blocks, aes(x = Block_Mid_Date, y = msa_value)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ylab("5-Year Error (%)") +
    xlab("Year (5-Year Block Midpoint)") +
    theme_bw() +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "5-Year block Median Symmetric Accuracy (MSA) over time",
                   paste0(unique(crossval_results$test_type)[1], " 5-year block MSA over time")),
            subtitle = sub_text) +
    theme(
      strip.background = element_rect(fill = "gray95"),
      panel.grid.minor = element_blank()
    )
  
  if (single_model) {
    p_msa_5yr_block <- p_msa_5yr_block + 
      labs(caption = paste0("Model: ", single_model_name, 
                            "\nEach point represents a distinct, non-overlapping 5-year block pooled together.")) +
      facet_grid(data_source ~ stock_spp_label, scales = "free_y")
  } else {
    p_msa_5yr_block <- p_msa_5yr_block + 
      labs(caption = "Each point represents a distinct, non-overlapping 5-year block pooled together.") +
      ggh4x::facet_grid2(data_source ~ stock_spp_label + Model, independent = "y", scales = "free_y")
  }
  
  print(p_msa_5yr_block)
  filename_msa_5yr <- paste0("./plots/crossval_msa_5year_blocks_", test_lbl, "_", file_tag, ".jpg")
  ggsave(plot = p_msa_5yr_block, filename = filename_msa_5yr, width = 16, height = 10, dpi = 300)
  
  # ============================================================================
  # 1.2 Pre-calculate and plot 5-Year Static Block Symmetric Signed Bias (SSPB)
  # ============================================================================
  sspb_blocks <- crossval_err_s %>%
    mutate(
      Year = lubridate::year(YW),
      Block_Start = (Year %/% 5) * 5
    ) %>%
    group_by(stock_spp_label, data_source, Model, Block_Start) %>%
    summarise(
      sspb_value = calc_sspb(predicted = pred_Wobs_s_test, observed = Wobs_s),
      .groups = "drop"
    ) %>%
    mutate(
      Block_Mid_Date = as.Date(paste0(Block_Start + 2, "-07-01"))
    )
  
  p_sspb_5yr_block <-
    ggplot(sspb_blocks, aes(x = Block_Mid_Date, y = sspb_value)) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 2) +
    ylab("5-Year Bias (%)") +
    xlab("Year (5-Year Block Midpoint)") +
    theme_bw() +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "5-Year block Symmetric Signed Percentage Bias (SSPB) over time",
                   paste0(unique(crossval_results$test_type)[1], " 5-year block SSPB over time")),
            subtitle = sub_text) +
    theme(
      strip.background = element_rect(fill = "gray95"),
      panel.grid.minor = element_blank()
    )
  
  if (single_model) {
    p_sspb_5yr_block <- p_sspb_5yr_block + 
      labs(caption = paste0("Model: ", single_model_name, 
                            "\nPositive values indicate over-prediction; negative values indicate under-prediction.")) +
      facet_grid(data_source ~ stock_spp_label, scales = "free_y")
  } else {
    p_sspb_5yr_block <- p_sspb_5yr_block + 
      labs(caption = "Positive values indicate over-prediction; negative values indicate under-prediction.") +
      ggh4x::facet_grid2(data_source ~ stock_spp_label + Model, independent = "y", scales = "free_y")
  }
  
  print(p_sspb_5yr_block)
  filename_sspb_5yr <- paste0("./plots/crossval_sspb_5year_blocks_", test_lbl, "_", file_tag, ".jpg")
  ggsave(plot = p_sspb_5yr_block, filename = filename_sspb_5yr, width = 16, height = 10, dpi = 300)
  
  # ============================================================================
  # 2. Plot percent error vs weight
  # ============================================================================
  p_ew <-
    ggplot(crossval_err_s, aes(x = Wobs_s, y = 100 * (pred_Wobs_s_test/Wobs_s - 1 ))) +
    geom_point(alpha = 0.5, aes(color = 100 * (pred_Wobs_s_test/Wobs_s - 1 ))) +
    geom_hline(yintercept = 0, linetype = 2) +
    ggh4x::facet_grid2(data_source ~ stock_spp_label + Model, independent = "all", scales = "free") +
    ylab("Error (%)") +
    theme_bw()  +
    labs(caption = "Positive errors indicate over-prediction.") +
    xlab("Observed weight (kg)") +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "Time series cross-validation error by weight",
                   paste0(unique(crossval_results$test_type)[1], " error by weight")),
            subtitle = sub_text) +
    theme(legend.title = element_blank()) +
    scale_color_gradient2(
      low = "darkred", 
      mid = "gray95", 
      high = "darkblue", 
      midpoint = 0, 
      trans = "pseudo_log"
    )
  
  print(p_ew)
  filename_ew <- paste0("./plots/crossval_error_weight_", test_lbl, "_", file_tag, ".jpg")
  ggsave(plot = p_ew, filename = filename_ew, width = 16, height = 10, dpi = 300)
  
  # ============================================================================
  # Data Prep for Observed vs Predicted & Error Horizons
  # ============================================================================
  crossval_results_op <-
    crossval_results %>%
    select(s, Wobs_s, pred_Wobs_s_test, Model, YW, data_source, fold, Ltru_i, stock_spp_label) %>%
    group_by(stock_spp_label, s) %>%
    mutate(Ltru_i_mean4port = ifelse(data_source == "Port aggregate", mean(Ltru_i), Ltru_i)) %>%
    distinct(stock_spp_label, s, Wobs_s, pred_Wobs_s_test, Model, YW, data_source, fold, Ltru_i_mean4port) %>%
    group_by(stock_spp_label, data_source) %>%
    mutate(lbin = cut(Ltru_i_mean4port,
                      breaks = quantile(Ltru_i_mean4port, 
                                        probs = seq(0, 1, .2)) - c(0.0001, rep(0, 5))
    )) %>%
    {
      if(grepl("Time series", unique(crossval_results$test_type)[1])) {
        group_by(., stock_spp_label, fold) %>%
          mutate(peel_start = min(lubridate::year(YW)) %>% as.character,
                 `Forecast horizon (years)` = as.numeric((YW - min(YW))/365))
      } else {.}
    }
  
  # ============================================================================
  # 3. Plot Observed vs Predicted by Length Quintile (All Data Sources Combined)
  # ============================================================================
  p_op_lbin <-
    ggplot(crossval_results_op) +
    geom_abline(linewidth = 0.5) +
    # Updated facet logic to perfectly force lbins into 5 fixed columns 
    # and stock/data_source combinations into rows
    ggh4x::facet_grid2(stock_spp_label + data_source ~ lbin, independent = "all", scales = "free") +
    theme_bw() +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "Time series cross-validation observed vs. predicted weight by length quintile",
                   paste0(unique(crossval_results$test_type)[1], 
                          " observed vs. predicted weight by length quintile")),
            subtitle = paste0(sub_text, "\n(90% highest density region)")) +
    xlab("Observed weight") +
    ylab("Predicted weight") +
    labs(caption = "Port aggregate length quintiles are based on mean sample length.") +
    ggdensity::geom_hdr(aes(x = Wobs_s, y = pred_Wobs_s_test, color = Model), 
                        probs = 0.9, fill = NA,
                        show.legend = c(color = TRUE, alpha = FALSE))
  
  print(p_op_lbin)
  
  filename_op_lbin <- paste0("./plots/crossval_obspred_quintiles_ALL_", test_lbl,
                             "_", file_tag, ".jpg")
  ggsave(plot = p_op_lbin, filename = filename_op_lbin, width = 18, height = 12, dpi = 300)
  
  
  # ============================================================================
  # 4. Plot mean predicted weight vs observed overall
  # ============================================================================
  p_op <-
    ggplot(crossval_results_op) +
    geom_abline(linewidth = 0.5) +
    ggh4x::facet_grid2(data_source ~ stock_spp_label, independent = "all", scales = "free") +
    theme_bw() +
    ggtitle(ifelse(grepl("Time series", unique(crossval_results$test_type)[1]),
                   "Time series cross-validation observed vs. predicted weight",
                   paste0(unique(crossval_results$test_type)[1], 
                          " observed vs. predicted weight")),
            subtitle = paste0(sub_text, "\n(90% highest density region)")) +
    xlab("Observed weight") +
    ylab("Predicted weight") +
    ggdensity::geom_hdr(aes(x = Wobs_s, y = pred_Wobs_s_test, color = Model), 
                        probs = 0.9, fill = NA,
                        show.legend = c(color = TRUE, alpha = FALSE))
  
  print(p_op)
  filename_op <- paste0("./plots/crossval_obspred_overall_", test_lbl, "_", file_tag, ".jpg")
  ggsave(plot = p_op, filename = filename_op, width = 14, height = 9, dpi = 300)
  
  # ============================================================================
  # 5. Optional Time-Series-Specific Error Metric Forecast Horizons
  # ============================================================================
  if(grepl("Time series", unique(crossval_results$test_type)[1])){
    crossval_results_tsmsa <-
      crossval_results %>%
      select(s, Wobs_s, pred_Wobs_s_test, Model, YW, data_source, fold, Ltru_i, stock_spp_label) %>%
      group_by(stock_spp_label, Model, fold) %>%
      mutate(peel_start = min(lubridate::year(YW)) %>% as.character,
             years_since_peel_start = round(as.numeric((YW - min(YW)))/365),
             ape = abs(100 * log(pred_Wobs_s_test/Wobs_s))) %>%
      group_by(stock_spp_label, Model, years_since_peel_start, data_source) %>%
      reframe(msa  = calc_msa(pred_Wobs_s_test, Wobs_s),
              msa_90ci = quantile(boot::boot(dat = data.frame(predicted = pred_Wobs_s_test,
                                                              observed  = Wobs_s),
                                             statistic=calc_msa_boot, R=1000)$t,
                                  probs = c(0.05, 0.95))
      ) %>%
      group_by(stock_spp_label, Model, years_since_peel_start, data_source) %>%
      mutate(msa_90lo = min(msa_90ci),
             msa_90hi = max(msa_90ci)) %>%
      distinct(across(-c(msa_90ci)))
    
    p_tsmsa <-
      ggplot(crossval_results_tsmsa,
             aes(x = years_since_peel_start, y = msa)) +
      geom_line(aes(color = Model)) +
      geom_ribbon(aes(ymin = msa_90lo, ymax = msa_90hi, fill = Model),
                  alpha = 0.2) +
      ggh4x::facet_grid2(data_source ~ stock_spp_label, independent = "y", scales = "free_y") +
      theme_bw() +
      ggtitle("Weight prediction error with increasing forecast horizon") +
      labs(caption = "Forecast horizon is rounded to the nearest year.\nThe line is the median error; the shaded interval is the 90% confidence interval.") +
      xlab("Forecast horizon (years)") +
      ylab("Error (%)")
    
    print(p_tsmsa)
    filename_tsmsa <- paste0("./plots/crossval_forecast_horizon_error_", file_tag, ".jpg")
    ggsave(plot = p_tsmsa, filename = filename_tsmsa, width = 14, height = 9, dpi = 300)
  }
  
}