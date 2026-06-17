plot_obsfit_ts <- function(mod, dat) {


  dat_wfit <-
    dat %>%
    ungroup() %>%
    mutate(W_i_det = mod$rep$log_W_i_det %>% exp) %>%
    #filter(data_source %in% c("NMFS BTS", "Port DMF")) %>%
    group_by(data_source, YEAR, MONTH, WEEK, LOC, s, Wobs_s) %>%
    reframe(Ltru_i_mean4port = ifelse(data_source == "Port", mean(Ltru_i), Ltru_i),
            Wpred_s = sum(W_i_det)) %>%
    group_by(data_source) %>%
    # Break up observations into quintiles
    filter(!is.na(Wobs_s)) %>%
    mutate(lbin = cut(Ltru_i_mean4port,
                      breaks = quantile(unique(Ltru_i_mean4port), probs = seq(0, 1, .2)) -
                        c(0.0001, rep(0, 5)))) %>%
    mutate(beginning = lubridate::ymd(stringr::str_c(YEAR, "-01-01")),
           date = beginning + lubridate::weeks(WEEK)) %>%
    rename(`data source` = data_source) %>%
    mutate(error_direction = ifelse((Wpred_s - Wobs_s) > 0, "Fit too high", "Fit too low"))
  
  for(i in unique(dat_wfit$`data source`)) {
    # --- Data Preparation ---
    # 1. Filter the original wide data for the segments
    plot_data_wide <- dat_wfit %>% filter(`data source` == i)
    
    # 2. Create a LONG version of the data for the points
    plot_data_long <- plot_data_wide %>%
      # Reshape from two columns (Wobs_s, Wpred_s) into two new columns
      tidyr::pivot_longer(
        cols = c(Wobs_s, Wpred_s),
        names_to = "point_type",
        values_to = "weight"
      ) %>%
      # Make the legend labels user-friendly
      mutate(point_type = recode(point_type,
                                 "Wobs_s" = "Observed",
                                 "Wpred_s" = "Fit"))
    
    pos <- position_jitter(width = 2, height = 0, seed = 1)
    
    # --- Plotting ---
    p <-
      ggplot() +  # Start with a blank ggplot canvas
      geom_point(
        data = plot_data_long,
        aes(x = date, y = weight, shape = point_type), # Map shape to the point type
        size = 1, alpha = 0.8,
        position = pos  # Apply jitter to the points
      ) +
      geom_segment(
        data = plot_data_wide,
        aes(x = date, y = Wobs_s, yend = Wpred_s, xend = date, color = error_direction),
        position = pos # Apply the SAME jitter to the segments
      ) +
      scale_color_manual(
        values = c("Fit too high" = "blue", "Fit too low" = "red"),
        name = "Error"
      ) +
      scale_shape_manual(
        values = c("Observed" = 16, "Fit" = 4), # 16 = circle, 4 = x
        name = ""
      ) +
      
      facet_wrap(~lbin, scales = "free_y") +
      ylab("Weight (kg)") +
      xlab("") +
      theme_bw() +
      ggtitle(paste0("Error between observed and fit weight by length quintile (", i, ")"),
              subtitle = paste0(unique(dat$stock_label), " ",
                                unique(dat$species_label))) +
      theme(text = element_text(size = 14))
    
    print(p)
    
    # Construct a dynamic filename
    filename <- paste0("./plots/obsfit_ts_", i, "_",  unique(dat$stock_label),
                       " ", unique(dat$species_label),
                       ".jpg")
    # Save the plot
    ggsave(plot = p, filename, w = 13, h = 9)
    
    message("Plot saved as ", filename)
  }
    
  
}