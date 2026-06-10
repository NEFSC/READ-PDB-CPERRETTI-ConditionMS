#' Plot Decadal Average Seasonal Weight-at-Length (Strict Week-of-Estimate Filter)
#'
#' @param mods A list of fitted model objects.
#' @param dat The original input data frame used for the model.
#' @param date_range A two-element vector specifying the start and end dates. 
#' @export
plot_wal_decadal <- function(mods, dat, date_range = c(NA, NA)) {
  
  # 1. Pull out pred_specs and representative length
  pred_specs <- mods[[1]]$pred_specs
  length_to_plot <- pred_specs$length_to_predict[
    which.min(abs(pred_specs$length_to_predict - median(pred_specs$length_to_predict)))]
  
  # Helper for robust seasonal date conversion
  get_dummy_date <- function(week_num) {
    as.Date(paste(2000, week_num, 1, sep = "-"), "%Y-%U-%u")
  }
  
  # 2. Helper to process predictions
  add_preds <- function(mod, pred_specs) {
    f_name <- ""
    if ("fold" %in% names(mod$test)) {
      t_type <- unique(mod$test$test_type); f_val  <- unique(mod$test$fold)
      if (grepl("series", t_type)) {
        f_name <- paste0(", leave out ", substr(max(dat$YW) - 365 * as.numeric(f_val) + 1, 1, 10), "+")
      } else if (grepl("eave-one-dataset-out", t_type)) {
        f_name <- paste0(", leave out ", unique(dat$data_source[dat$s %in% mod$test$s_test]))
      } else { f_name <- paste0(" fold ", f_val) }
    }
    
    preds_to_plot <- pred_specs %>%
      ungroup() %>%
      mutate(
        pred_wal = exp(mod$rep$pred_log_wal),
        fold_name = f_name,
        model_fold = paste0(mod$model_name, ": ", paste(mod$model_effects, collapse = " "), fold_name)
      )
    return(preds_to_plot)
  }
  
  # 3. Process Raw Data and Create Strict Year-Week Mask
  dat_processed <- dat %>%
    mutate(
      # Standardize data source names
      data_source = case_when(
        data_source == "NMFS BTS" ~ "Survey",
        data_source == "Port"     ~ "Port aggregate",
        data_source == "Port DMF" ~ "Port individual",
        TRUE                      ~ data_source
      ),
      year = lubridate::year(YW),
      week = lubridate::week(YW),
      dummy_date_precise = as.Date(format(YW, "2000-%m-%d")),
      decade = paste0(floor(year / 10) * 10, "s")
    ) %>%
    { if(!all(is.na(date_range))) 
      filter(., YW >= as.Date(date_range[1]) & YW <= as.Date(date_range[2])) 
      else . }
  
  # Mask based on Year and Week
  year_week_mask <- dat_processed %>%
    distinct(year, week) %>%
    mutate(has_data = TRUE)
  
  # 4. Process Predictions and Apply Strict Filter
  all_preds <- purrr::map_df(.x = mods, .f = add_preds, pred_specs) %>%
    filter(length_to_predict %in% length_to_plot)
  
  processed_preds_filtered <- all_preds %>%
    mutate(
      year = lubridate::year(yw_to_predict),
      week = lubridate::week(yw_to_predict),
      dummy_date = get_dummy_date(week),
      decade = paste0(floor(year / 10) * 10, "s")
    ) %>%
    inner_join(year_week_mask, by = c("year", "week"))
  
  # 5. Calculate Means
  decadal_seasonal <- processed_preds_filtered %>%
    group_by(decade, week, dummy_date, model_fold) %>%
    summarise(decadal_avg_wal = mean(pred_wal, na.rm = TRUE), .groups = "drop") %>%
    group_by(decade, model_fold) %>%
    tidyr::complete(week = 1:52) %>% 
    mutate(dummy_date = get_dummy_date(week)) %>%
    ungroup()
  
  global_seasonal_mean <- processed_preds_filtered %>%
    group_by(week, dummy_date, model_fold) %>%
    summarise(global_avg_wal = mean(pred_wal, na.rm = TRUE), .groups = "drop") %>%
    group_by(model_fold) %>%
    tidyr::complete(week = 1:52) %>%
    mutate(dummy_date = get_dummy_date(week)) %>%
    ungroup()
  
  # --- SCALE CALCULATIONS ---
  y_data_min <- min(decadal_seasonal$decadal_avg_wal, na.rm = TRUE)
  y_data_max <- max(decadal_seasonal$decadal_avg_wal, na.rm = TRUE)
  y_range <- y_data_max - y_data_min
  
  rug_floor <- y_data_min - (y_range * 0.15)
  rug_ceiling <- y_data_min - (y_range * 0.05)
  
  dat_intensity <- dat_processed %>%
    group_by(decade, dummy_date_precise, data_source) %>%
    summarise(n_obs = n_distinct(Wobs_s), .groups = "drop") %>%
    rename(`Data source` = data_source) %>%
    mutate(rel_height = log10(n_obs + 1) / max(log10(n_obs + 1), na.rm = TRUE))
  
  # 6. Build the Plot
  p <- ggplot(decadal_seasonal, aes(x = dummy_date))
  
  # Dynamic Line/Point layering based on number of models
  if (length(mods) == 1) {
    p <- p + 
      geom_line(data = global_seasonal_mean, aes(y = global_avg_wal), 
                linetype = "dashed", alpha = 0.5, linewidth = 0.6, color = "black", na.rm = TRUE) +
      geom_line(aes(y = decadal_avg_wal), 
                linewidth = 1.1, na.rm = TRUE) +
      geom_point(aes(y = decadal_avg_wal), 
                 size = 1.6, alpha = 0.7, na.rm = TRUE)
  } else {
    p <- p + 
      geom_line(data = global_seasonal_mean, aes(y = global_avg_wal, color = model_fold), 
                linetype = "dashed", alpha = 0.3, linewidth = 0.6, na.rm = TRUE) +
      geom_line(aes(y = decadal_avg_wal, color = model_fold), 
                linewidth = 1.1, na.rm = TRUE) +
      geom_point(aes(y = decadal_avg_wal, color = model_fold), 
                 size = 1.6, alpha = 0.7, na.rm = TRUE) +
      scale_color_brewer(palette = "Dark2", name = "Model")
  }
  
  # Add the Intensity Rug and Formatting
  p <- p +
    # INTENSITY RUG
    ggnewscale::new_scale_color() +
    geom_segment(data = dat_intensity, 
                 aes(x = dummy_date_precise, 
                     xend = dummy_date_precise, 
                     y = rug_floor, 
                     yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                     color = `Data source`,
                     alpha = n_obs),
                 linewidth = 0.6, 
                 inherit.aes = FALSE) +
    
    scale_color_brewer(palette = "Set1", name = "Data source") + 
    scale_alpha_continuous(range = c(0.4, 1), guide = "none") + 
    facet_wrap(~decade) + 
    scale_x_date(date_labels = "%b", date_breaks = "2 months") + 
    coord_cartesian(ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") + 
    theme_bw() +
    labs(
      title = paste0("Decadal mean weight-at-length (", length_to_plot, "cm)"),
      subtitle = paste(unique(dat$stock_label), unique(dat$species_label)),
      y = "Weight (kg)", x = "Month",
      caption = "Solid line: Decadal mean | Dashed line: Mean across all decades | Rug height: Data intensity"
    ) +
    theme(
      text = element_text(size = 14),
      strip.background = element_rect(fill = "grey95"),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 25, l = 10) 
    )
  
  #print(p)
  
  # Save
  if(!dir.exists("./plots")) dir.create("./plots")
  filename <- paste0("./plots/decadal_seasonal_strict_filter_",
                     unique(dat$stock_label), "_",
                     unique(dat$species_label), ".jpg")
  ggsave(plot = p, filename, width = 13, height = 9, dpi = 300)
  
  return(p)
}