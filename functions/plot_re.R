#' Plot Model Random Effects Over Time
#'
#' @description
#' This function visualizes estimated random effects from a fitted model. It plots
#' location, year, week, and year-week effects (if they were estimted)
#' against time and overlays a rug
#' plot to show the temporal distribution of the underlying data sources.
#'
#' @details
#' The function first extracts effect estimates (e.g., `d_loc`, `d_y`, `d_w`, 
#' `d_yw`, `d_sem2`)
#' from the model's report list (`mod$rep`). It then reshapes this data into a
#' long format suitable for `ggplot2`. The final plot is displayed on the active
#' graphics device and also saved as a JPEG file in the `./plots/` directory.
#'
#' @param mod A fitted model object from RTMB, which must contain the
#'   report list (`rep`) and parameter estimates (`pl`, `plsd`).
#' @param dat The input data frame used to fit the model. It must contain columns
#'   like `data_source`, `YW`, `stock_label`, and `species_label`.
#' @param date_range A two-element vector specifying the start and end dates
#'   (e.g., `c("2020-01-01", "2023-12-31")`) to set the x-axis limits.
#'   Defaults to `c(NA, NA)`, which allows `ggplot` to set the range automatically.
#'
#' @return This function is called for its side effects. It prints a `ggplot`
#'   object and saves it to a file. It does not return a value.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
#'
plot_re <- function(mods, dat, date_range = c(NA, NA)) {
  
  # --- 1. Input Validation ---
  if(is.null(dat)) {
    stop("Argument 'dat' must be provided.")
  }
  
  # --- 1.5. Standardize Data Source Labels ---
  dat <- dat %>%
    mutate(data_source = case_when(
      data_source == "NMFS BTS" ~ "Survey",
      data_source == "Port"     ~ "Port aggregate",
      data_source == "Port DMF" ~ "Port individual",
      TRUE                      ~ data_source
    ))
  
  # --- 2. Extract Estimates ---
  ests <- purrr::map_dfr(mods, pull_re)
  
  # --- 3. Process Intensity Rug Data ---
  dat_rug_intensity <- dat %>%
    { if(!all(is.na(date_range))) filter(., YW >= as.Date(date_range[1]) & YW <= as.Date(date_range[2])) else . } %>%
    group_by(YW, data_source) %>%
    summarise(n_obs = n_distinct(Wobs_s), .groups = "drop") %>%
    rename(`Data source` = data_source) %>%
    mutate(rel_height = log10(n_obs + 1) / max(log10(n_obs + 1), na.rm = TRUE))
  
  # --- 4. Scale Calculations for Rug Positioning ---
  y_data_min <- min(ests$value, na.rm = TRUE)
  y_data_max <- max(ests$value, na.rm = TRUE)
  y_range <- y_data_max - y_data_min
  
  # Define the "basement" for the rug segments
  rug_floor <- y_data_min - (y_range * 0.15)
  rug_ceiling <- y_data_min - (y_range * 0.05)
  
  # Determine limits conditionally
  x_lims <- if(!all(is.na(date_range))) as.Date(date_range) else NULL
  
  # --- 5. Create the plots ####################################################
  
  if(length(unique(ests$Model)) == 1) {
    p <-
      ggplot(ests, aes(x = YW, y = value, color = effect)) +
      geom_line() +
      geom_point() +
      geom_hline(aes(yintercept = 0), linetype = 2, color = "black") +
      
      # Add Intensity Rug (Harmonized)
      ggnewscale::new_scale_color() +
      geom_segment(data = dat_rug_intensity, 
                   aes(x = as.Date(YW), xend = as.Date(YW), y = rug_floor, 
                       yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                       color = `Data source`, alpha = n_obs),
                   linewidth = 0.5, inherit.aes = FALSE) +
      scale_color_brewer(palette = "Set1") +
      scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
      
      # Fixed X-Axis Labels
      scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.02, 0.02)) +
      coord_cartesian(xlim = x_lims, ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") +
      ylab("Estimate") +
      xlab("Year") +
      theme_bw() +
      ggtitle("Effect time series",
              subtitle = paste0(unique(dat$stock_label), " ",
                                unique(dat$species_label))) +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1), # Angled text
            legend.position = "bottom",
            legend.box = "vertical",
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10)) +
      labs(caption = paste0("Model effects: ", unique(ests$Model),
                            "\nRug height: Data intensity"))
    
    # Print the plot to the graphics device
    print(p)
  }
  
  if(length(unique(ests$Model)) > 1) {
    p <-
      ggplot(ests, aes(x = YW, y = value, color = Model)) +
      geom_line() +
      geom_point() +
      facet_wrap(~effect) +
      geom_hline(aes(yintercept = 0), linetype = 2, color = "black") +
      
      # Add Intensity Rug (Harmonized)
      ggnewscale::new_scale_color() +
      geom_segment(data = dat_rug_intensity, 
                   aes(x = as.Date(YW), xend = as.Date(YW), y = rug_floor, 
                       yend = rug_floor + ((rug_ceiling - rug_floor) * rel_height),
                       color = `Data source`, alpha = n_obs),
                   linewidth = 0.5, inherit.aes = FALSE) +
      scale_color_brewer(palette = "Set1") +
      scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
      
      # Fixed X-Axis Labels
      scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.02, 0.02)) +
      coord_cartesian(xlim = x_lims, ylim = c(rug_floor, y_data_max + (y_range * 0.05)), clip = "off") +
      ylab("Estimate") +
      xlab("Year") +
      theme_bw() +
      ggtitle("Effect time series",
              subtitle = paste0(unique(dat$stock_label), " ",
                                unique(dat$species_label))) +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1), # Angled text
            legend.position = "bottom",
            legend.box = "vertical",
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10)) +
      labs(caption = "Rug height: Data intensity")
    
    # Print the plot to the graphics device
    print(p)
  }
  
  # --- 6. Save the plot to a file ---
  if(!dir.exists("./plots")) dir.create("./plots")
  filename <- paste0("./plots/re_estimates_",  unique(dat$stock_label),
                     "_", unique(dat$species_label),
                     ".jpg")
  
  ggsave(plot = p, filename, w = 12, h = 9, dpi = 300)
  
  message("Plot saved as ", filename)
  
}