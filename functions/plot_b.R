#' Plot Time Series of b Parameter Estimates
#'
#' @param mods A list of fitted model objects.
#' @param dat The raw data frame used for creating the rug plot. Must contain
#'   columns `data_source`, `YW`, and `s`.
#' @param date_range A vector of two dates.
#' @param plot_CI Logical. Whether to plot the 90% confidence interval.
#' @param loc Character or Numeric. Optional. The statistical area location.
plot_b <- function(mods, dat = NULL, date_range = c(NA, NA), plot_CI = TRUE, 
                   loc = NULL) {
  
  # --- 1. Input Validation ---
  if(is.null(dat)) {
    stop("Argument 'dat' must be provided.")
  }
  if(any(!is.na(date_range)) && (as.Date(date_range[1]) > as.Date(date_range[2]))) {
    stop("Start date must be before end date.")
  }
  
  # --- 1.5 Standardize data source labels ---
  dat <- dat %>%
    mutate(data_source = case_when(
      data_source == "NMFS BTS" ~ "Survey",
      data_source == "Port"     ~ "Port aggregate",
      data_source == "Port DMF" ~ "Port individual",
      TRUE                      ~ data_source
    ))
  
  # --- 2. Data Preparation ---
  ests <- purrr::map_df(.x = mods, .f = pull_ests, dat, loc = loc)
  
  # --- 3. Process Intensity Rug Data (Harmonized with plot_wal) ---
  dat_rug_intensity <- dat %>%
    { if(!all(is.na(date_range))) filter(., YW >= as.Date(date_range[1]) & YW <= as.Date(date_range[2])) else . } %>%
    group_by(YW, data_source) %>%
    summarise(n_obs = n_distinct(Wobs_s), .groups = "drop") %>%
    rename(`Data source` = data_source) %>%
    mutate(rel_height = log10(n_obs + 1) / max(log10(n_obs + 1), na.rm = TRUE))
  
  # --- 4. Scale Calculations for Rug Positioning ---
  y_data_min <- min(ests$value[ests$variable == "b"], na.rm = TRUE)
  y_data_max <- max(ests$value[ests$variable == "b"], na.rm = TRUE)
  y_range <- y_data_max - y_data_min
  
  # Define the "basement" for the rug segments
  rug_floor <- y_data_min - (y_range * 0.15)
  rug_ceiling <- y_data_min - (y_range * 0.05)
  
  # --- 5. Make single model plot #############################################
  if(length(unique(ests$model_fold)) == 1) {
    p <- ggplot(ests %>% filter(variable == "b"), aes(x = as.Date(YW), y = value)) +
      geom_line(aes(color = lubridate::month(YW)), linewidth = 0.8) +
      geom_text(aes(label = lubridate::month(YW), color = lubridate::month(YW)), 
                size = 3, check_overlap = TRUE) +
      scale_color_gradient2(low = "darkblue", mid = "red", high = "darkgreen", 
                            midpoint = 7, name = "Month") +
      {if(plot_CI) geom_ribbon(aes(ymin = CI_90lo, ymax = CI_90hi), alpha = 0.2)} +
      
      # Add Intensity Rug (Harmonized)
      ggnewscale::new_scale_color() +
      geom_segment(data = dat_rug_intensity, 
                   aes(x = as.Date(YW), xend = as.Date(YW), y = rug_floor, 
                       yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                       color = `Data source`, alpha = n_obs),
                   linewidth = 0.5, inherit.aes = FALSE) +
      scale_color_brewer(palette = "Set1") +
      scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
      
      geom_hline(aes(yintercept = mean(value, na.rm = TRUE)), linetype = 2, color = "black") +
      # Enforce 2-year ticks
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      coord_cartesian(ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") +
      theme_bw() +
      {if("area" %in% mods[[1]]$model_effects) facet_wrap(~LOC)} +
      labs(y = "*b*", x = "Year",
           title = "*b* estimate (W = aL<sup>b</sup>)",
           subtitle = paste0(unique(dat$stock_label), " ", unique(dat$species_label),
                             ifelse(!is.null(loc), paste0(" | Statistical area ", loc), "")),
           caption = paste0(ifelse(plot_CI, "Shaded area: 90% CI | ", ""), 
                            "Dashed line: mean | Rug height: Data intensity\nModel: ", unique(ests$model_fold))) +
      theme(plot.title = ggtext::element_markdown(),
            axis.title.y = ggtext::element_markdown(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            legend.box = "vertical", # Stacks legends nicely
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10)) # Increased bottom margin
  }
  
  # --- 6. Make multi-model plot ##############################################
  if(length(unique(ests$model_fold)) > 1) {
    p <- ggplot(ests %>% filter(variable == "b"), aes(x = as.Date(YW), y = value, color = model_fold)) +
      geom_line(linewidth = 0.8) +
      {if(plot_CI) geom_ribbon(aes(ymin = CI_90lo, ymax = CI_90hi, fill = model_fold), 
                               alpha = 0.1, color = NA)} +
      scale_color_brewer(palette = "Dark2", name = "Model") +
      scale_fill_brewer(palette = "Dark2", guide = "none") +
      
      # Add Intensity Rug (Harmonized)
      ggnewscale::new_scale_color() +
      geom_segment(data = dat_rug_intensity, 
                   aes(x = as.Date(YW), xend = as.Date(YW), y = rug_floor, 
                       yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                       color = `Data source`, alpha = n_obs),
                   linewidth = 0.5, inherit.aes = FALSE) +
      scale_color_brewer(palette = "Set1") +
      scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
      
      # Enforce 2-year ticks
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      coord_cartesian(ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") +
      theme_bw() +
      {if(any(grepl("area", ests$model_fold))) facet_wrap(~LOC)} +
      labs(y = "*b*", x = "Year",
           title = "*b* estimate (W = aL<sup>b</sup>)",
           subtitle = paste0(unique(dat$stock_label), " ", unique(dat$species_label),
                             ifelse(!is.null(loc), paste0(" | Statistical area ", loc), "")),
           caption = paste0("Solid: Estimated Trend | Dashed: Seasonal Avg | Rug height: Data intensity",
                            ifelse(plot_CI, "\nShaded area: 90% Confidence Interval", ""))) +
      theme(plot.title = ggtext::element_markdown(),
            axis.title.y = ggtext::element_markdown(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            legend.box = "vertical", # Stacks legends nicely
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10)) # Increased bottom margin
  }
  
  # --- 7. Save and Return ---
  if(!dir.exists("./plots")) dir.create("./plots")
  filename <- paste0("./plots/b_est_ts_", unique(dat$stock_label), "_", 
                     unique(dat$species_label), ifelse(!is.null(loc), paste0("_", loc), ""), ".jpg")
  
  ggsave(plot = p, filename, width = 13, height = 9, dpi = 300)
  message("Plot saved as ", filename)
  return(p)
}