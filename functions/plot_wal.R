#' Plot Estimated Weight-at-Length Over Time (Combined Single/Multi View)
#'
#' @param mods A list of fitted model objects.
#' @param dat The original input data frame used for the model.
#' @param length_to_plot Numeric. The specific length (cm) to plot. 
#' @param date_range A two-element vector specifying the start and end dates. 
#' @param loc Character or Numeric. Optional. The statistical area location.
#' @param plot_CI Logical. Whether to plot the 90% confidence interval.
#' @export
plot_wal <- function(mods, dat, length_to_plot = NULL, date_range = c(NA, NA), 
                     loc = NULL, plot_CI = FALSE) {
  
  # 1. Setup - Pull pred_specs and validate
  # If 'mods' is a single model object, wrap it in a list for mapping
  if ("obj" %in% names(mods)) mods <- list(mods)
  
  pred_specs_base <- mods[[1]]$pred_specs
  if (is.null(pred_specs_base)) stop("Error: 'pred_specs' not found.")
  
  target_loc <- loc
  
  # --- 1.5 Standardize data source labels ---
  dat <- dat %>%
    dplyr::mutate(data_source = dplyr::case_when(
      data_source == "NMFS BTS" ~ "Survey",
      data_source == "Port"     ~ "Port aggregate",
      data_source == "Port DMF" ~ "Port individual",
      TRUE                      ~ data_source # Keeps any other names unchanged
    ))
  
  # Determine target length
  if (is.null(length_to_plot)) {
    target_len <- median(dat$Ltru_i, na.rm = TRUE)
  } else {
    target_len <- length_to_plot
  }
  
  selected_length <- pred_specs_base$length_to_predict[
    which.min(abs(pred_specs_base$length_to_predict - target_len))]
  
  # 2. Internal helper to process predictions, CIs, and Fold Labels
  add_preds <- function(mod, full_p_specs, t_loc) {
    if (is.null(mod$rep$pred_log_wal)) stop("Error: 'pred_log_wal' not found.")
    
    # --- Calculate Fold Name based on Data Source ---
    f_name <- ""
    if ("fold" %in% names(mod$test)) {
      t_type <- unique(mod$test$test_type)
      f_val  <- unique(mod$test$fold)
      
      if (grepl("series", t_type)) {
        f_name <- paste0(", leave out ", substr(max(dat$YW) - 365 * as.numeric(f_val) + 1, 1, 10), "+")
      } else if (grepl("eave-one-dataset-out", t_type)) {
        # Look up which data source was in the test set
        held_out_name <- dat %>%
          dplyr::filter(s %in% mod$test$s_test) %>%
          dplyr::pull(data_source) %>%
          unique()
        f_name <- paste0(", leave out ", paste(held_out_name, collapse = ", "))
      } else { 
        f_name <- paste0(" fold ", f_val) 
      }
    }
    
    # Base Predictions in Log Space
    preds <- full_p_specs %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        val_log = as.numeric(mod$rep$pred_log_wal),
        month = lubridate::month(yw_to_predict),
        week_to_predict = lubridate::week(yw_to_predict),
        fold_name = f_name, 
        model_fold = paste0(mod$model_name, ": ", paste(mod$model_effects, collapse = " "), fold_name),
        se_log = NA_real_
      )
    
    # Map Standard Errors if they exist (pred_log_wal_sdrep)
    if (!is.null(mod$plsd$pred_log_wal_sdrep)) {
      mask <- rep(TRUE, nrow(full_p_specs))
      # Ensure mod$adrep was saved during fit_lw
      sdrep_pred <- mod$sdrep_pred
      
      if (!is.null(sdrep_pred$loc))    mask <- mask & (full_p_specs$loc_to_predict %in% sdrep_pred$loc)
      if (!is.null(sdrep_pred$year))   mask <- mask & (full_p_specs$year_to_predict %in% sdrep_pred$year)
      if (!is.null(sdrep_pred$length)) mask <- mask & (full_p_specs$length_to_predict %in% sdrep_pred$length)
      
      match_idx <- which(mask)
      if (length(match_idx) == length(mod$plsd$pred_log_wal_sdrep)) {
        preds$se_log[match_idx] <- as.numeric(mod$plsd$pred_log_wal_sdrep)
      }
    }
    
    # Transform to real scale
    preds <- preds %>%
      dplyr::mutate(
        pred_wal = exp(val_log),
        CI_90lo  = exp(val_log - 1.645 * se_log),
        CI_90hi  = exp(val_log + 1.645 * se_log)
      )
    
    if(!is.null(t_loc)) preds <- preds %>% dplyr::filter(loc_to_predict == !!t_loc)
    return(preds)
  }
  
  # 3. Combine models
  preds_to_plot <- purrr::map_df(.x = mods, .f = ~add_preds(.x, pred_specs_base, target_loc)) %>%
    dplyr::filter(length_to_predict %in% selected_length)
  
  if(nrow(preds_to_plot) == 0) stop("Filtering resulted in 0 rows. Check your 'loc' or 'length' inputs.")
  
  if(!all(is.na(date_range))) {
    preds_to_plot <- preds_to_plot %>%
      dplyr::filter(yw_to_predict >= as.Date(date_range[1]) & yw_to_predict <= as.Date(date_range[2]))
  }
  
  # 4. Process Intensity Rug Data
  dat_rug_intensity <- dat %>%
    { if(!all(is.na(date_range))) dplyr::filter(., YW >= as.Date(date_range[1]) & YW <= as.Date(date_range[2])) else . } %>%
    dplyr::group_by(YW, data_source) %>%
    dplyr::summarise(n_obs = n_distinct(Wobs_s), .groups = "drop") %>%
    dplyr::rename(`Data source` = data_source) %>%
    dplyr::mutate(rel_height = log10(n_obs + 1) / max(log10(n_obs + 1), na.rm = TRUE))
  
  # 5. Seasonal Means for dashed trend
  seasonal_means <- preds_to_plot %>%
    dplyr::group_by(model_fold, week_to_predict) %>%
    dplyr::summarise(seasonal_avg_wal = mean(pred_wal, na.rm = TRUE), .groups = "drop")
  
  preds_to_plot <- preds_to_plot %>% 
    dplyr::left_join(seasonal_means, by = c("model_fold", "week_to_predict"))
  
  # --- SCALE CALCULATIONS ---
  y_data_min <- min(preds_to_plot$pred_wal, na.rm = TRUE)
  y_data_max <- max(preds_to_plot$pred_wal, na.rm = TRUE)
  y_range    <- y_data_max - y_data_min
  rug_floor  <- y_data_min - (y_range * 0.15)
  rug_ceiling <- y_data_min - (y_range * 0.05)
  
  # 6. Build Plot
  has_CI <- plot_CI && any(!is.na(preds_to_plot$CI_90lo))
  
  p <- ggplot(preds_to_plot, aes(x = as.Date(yw_to_predict))) +
    geom_line(aes(y = seasonal_avg_wal, color = if(length(mods) > 1) model_fold else NULL), 
              linetype = "dashed", alpha = 0.4)
  
  # Optional CI Ribbon
  if(has_CI) {
    if(length(mods) == 1) {
      p <- p + geom_ribbon(aes(ymin = CI_90lo, ymax = CI_90hi), alpha = 0.2, fill = "grey50")
    } else {
      p <- p + geom_ribbon(aes(ymin = CI_90lo, ymax = CI_90hi, fill = model_fold), alpha = 0.1, color = NA) +
        scale_fill_brewer(palette = "Dark2", guide = "none")
    }
  }
  
  # Main Line Layers
  if(length(mods) == 1) {
    p <- p + 
      # Hide the line from the legend to avoid drawing a strikethrough over the numbers
      geom_line(aes(y = pred_wal, color = month), linewidth = 0.8, show.legend = FALSE) +
      # Use key_glyph = "text" to tell ggplot to render text inside the legend keys
      geom_text(aes(y = pred_wal, label = month, color = month), 
                size = 3, check_overlap = TRUE, key_glyph = "text") +
      scale_color_gradient2(
        low = "darkblue", mid = "red", high = "darkgreen", midpoint = 7, 
        breaks = 1:12, 
        name = "Month:",
        guide = guide_legend(
          nrow = 1,
          label = FALSE,             # Suppress the default black text next to the legend key
          title.position = "left",   # Put the word "Month:" to the left of the numbers
          title.vjust = 0.5,
          override.aes = list(
            label = as.character(1:12), # Force the keys to display 1 through 12
            size = 5,                   # Increase font size for readability in the legend
            fontface = "bold"
          )
        )
      )
  } else {
    p <- p + 
      geom_line(aes(y = pred_wal, color = model_fold), linewidth = 0.8) +
      scale_color_brewer(palette = "Dark2", name = "Model", guide = guide_legend(ncol = 1))
  }
  
  # Add Intensity Rug
  p <- p + 
    ggnewscale::new_scale_color() +
    geom_segment(data = dat_rug_intensity, 
                 aes(x = as.Date(YW), xend = as.Date(YW), y = rug_floor, 
                     yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                     color = `Data source`, alpha = n_obs),
                 linewidth = 0.5, inherit.aes = FALSE) +
    scale_color_brewer(palette = "Set1") +
    scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
    theme_bw() +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0.02, 0.02)) + 
    coord_cartesian(ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom", 
          legend.box = "vertical",
          plot.margin = margin(t = 10, r = 10, b = 40, l = 10)) +
    labs(y = "Weight (kg)", x = "",
         title = paste0("Estimated weight at ", selected_length, "cm"),
         subtitle = paste0(unique(dat$stock_label), " ", unique(dat$species_label),
                           ifelse(!is.null(target_loc), paste0(" | Statistical area ", target_loc), "")),
         # Caption gracefully drops the Shaded Area text if has_CI is FALSE
         caption = paste0("Solid: Estimated Trend | Dashed: Seasonal Avg | Rug height: Data intensity",
                          ifelse(has_CI, "\nShaded area: 90% Confidence Interval", "")))
  
  # 7. Save and Return
  if(!dir.exists("./plots")) dir.create("./plots")
  loc_suffix <- if(!is.null(target_loc)) paste0("_", target_loc) else ""
  filename <- paste0("./plots/wal_ts_", selected_length, "cm", loc_suffix, "_", 
                     unique(dat$stock_label), "_", unique(dat$species_label), ".jpg")
  
  ggsave(plot = p, filename, width = 13, height = 9, dpi = 300)
  
  message("Plot saved to: ", filename)
  return(p)
}