## Plot raw data in various ways ###############################################
plot_raw_data <- function(dat, save_plots = FALSE) {
  
  len2hist <-
    dat$bsm_dat %>% 
    select(YEAR, MONTH, LENGTH, NUMLEN) %>%
    mutate(data_source = "Port aggregate") %>% # Updated label
    bind_rows(dat$srv_dat %>% 
                select(PURPOSE_CODE, YEAR, MONTH, LENGTH) %>%
                mutate(data_source = ifelse(PURPOSE_CODE == 10, "Survey", NA), # Updated label
                       data_source = ifelse(PURPOSE_CODE == 11, "MADMF BTS", data_source),
                       NUMLEN = 1,
                       YEAR = as.integer(YEAR)) %>%
                select(data_source, YEAR, MONTH, LENGTH, NUMLEN)) %>%
    bind_rows(dat$dmf_dat %>% 
                filter(PARAM_DESCR %in% c("FORK LENGTH", "TOTAL LENGTH")) %>%
                select(SAMPLING_YR, MONTH, PARAM_VALUE_NUM) %>%
                mutate(NUMLEN = 1,
                       LENGTH = PARAM_VALUE_NUM/10,
                       data_source = "Port individual") %>% # Updated label
                rename(YEAR = SAMPLING_YR) %>%
                select(-PARAM_VALUE_NUM)) %>%
    rename(`Length (cm)` = LENGTH) %>%
    uncount(NUMLEN) %>%
    left_join(data.frame(MONTH = 1:12, 
                         month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"),
                                                levels = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"))
    ))
  
  # Over all months
  p_hist <- 
    ggplot(len2hist, aes(x = `Length (cm)`, color = data_source, fill = data_source)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    theme_bw() +
    ggtitle("Histogram of lengths by source",
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label), " (",
                              min(len2hist$YEAR), "-", 
                              max(len2hist$YEAR), ")")) +
    theme(legend.title = element_blank())
  
  print(p_hist)
  
  
  
  p_dens <- 
    ggplot(len2hist, aes(x = `Length (cm)`, color = data_source, fill = data_source)) +
    geom_density(alpha = 0.5, position = "identity") +
    theme_bw() +
    ggtitle("Probability density of lengths by source",
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label), " (",
                              min(len2hist$YEAR), "-", 
                              max(len2hist$YEAR), ")")) +
    theme(legend.title = element_blank())
  
  print(p_dens)
  
  # By month
  p_hist_month <-
    ggplot(len2hist, aes(x = `Length (cm)`, color = data_source, fill = data_source)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    facet_wrap(~month_display) +
    theme_bw() +
    ggtitle("Histogram of lengths by source",
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label), " (",
                              min(len2hist$YEAR), "-", 
                              max(len2hist$YEAR), ")")) +
    theme(legend.title = element_blank())
  
  print(p_hist_month)
  
  p_dens_month <- 
    ggplot(len2hist, aes(x = `Length (cm)`, color = data_source, fill = data_source)) +
    geom_density(alpha = 0.5, position = "identity") +
    facet_wrap(~month_display) +
    theme_bw() +
    ggtitle("Probability density of lengths by source",
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label), " (",
                              min(len2hist$YEAR), "-", 
                              max(len2hist$YEAR), ")")) +
    theme(legend.title = element_blank())
  
  print(p_dens_month)
  
  # Plot port weight samples
  samp2hist <- 
    dat$bsm_dat %>%
    uncount(NUMLEN) %>%
    group_by(LINK, TALLYNO, SPPLNDLB, YEAR, MONTH, DAY, WGTSAMP) %>%
    summarise(WGTSAMP = unique(WGTSAMP),
              NUMSAMP = unique(NUMSAMP),
              meanL   = mean(LENGTH)) %>%
    left_join(data.frame(MONTH = 1:12, 
                         month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"),
                                                levels = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"))
    ))
  
  # Weights over all months ####################################################
  # Calculate breaks for the lines
  highlight_breaks <- seq(0, max(samp2hist$WGTSAMP, na.rm = TRUE) + 100, by = 100)
  
  p_port_total_weights <- ggplot(samp2hist, aes(x = WGTSAMP)) +
    
    # 1. Add the vertical lines FIRST so they sit behind the data
    # Lightened the color so it doesn't obscure the 1-unit wide bar
    geom_vline(xintercept = highlight_breaks, 
               color = "grey60", 
               linetype = "dotted", 
               linewidth = 0.5,
               alpha = 0.5) +
    
    # 2. Histogram with binwidth = 1
    # Kept color = NA so borders don't crush the 1-unit width
    geom_histogram(aes(fill = after_stat(count > 0 & x %in% highlight_breaks)),
                   binwidth = 1, 
                   center = 0,
                   color = NA) + 
    
    # 3. High contrast colors: light grey for normal data, bright red for 100s
    scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "red")) +
    
    scale_x_continuous(breaks = highlight_breaks) +
    
    labs(
      title = "Histograms of Port Sample Weights",
      subtitle = paste0("Highlighting exact 100kg increments | ", 
                        paste0(unique(dat$srv_dat$stock_label), " ",
                               unique(dat$srv_dat$species_label), " (",
                               min(len2hist$YEAR), "-", 
                               max(len2hist$YEAR), ")")),
      x = "Sample Weight (kg)",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  print(p_port_total_weights)
  
  # Weights by month ###########################################################
  p_port_total_weights_by_month <-
    ggplot(samp2hist, aes(x = WGTSAMP)) +
    geom_histogram(position = "identity") +
    facet_wrap(~month_display) +
    ggtitle("Histograms of port sample weights",
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label), " (",
                              min(len2hist$YEAR), "-", 
                              max(len2hist$YEAR), ")")) +
    theme_bw() +
    xlab("Sample weight (kg)")
  
  print(p_port_total_weights_by_month)
  
  
  # Heatmap of the number of aggregate samples by month and year
  wgts2tile0 <-
    samp2hist %>%
    group_by(YEAR, MONTH) %>%
    summarise(`Number of weights` = n()) %>%
    mutate(data_source = "Port aggregate (sample weights)") %>% # Updated label
    bind_rows({len2hist %>%
        filter(data_source %in% c("Survey", "MADMF BTS", "Port individual")) %>% # Updated filter
        mutate(data_source = ifelse(data_source == "Survey", 
                                    "Survey (individual weights)", 
                                    data_source),
               data_source = ifelse(data_source == "MADMF BTS", 
                                    "MADMF BTS (individual weights)",
                                    data_source),
               data_source = ifelse(data_source == "Port individual", 
                                    "Port individual (individual weights)",
                                    data_source)) %>%
        group_by(YEAR, MONTH, data_source) %>%
        summarise(`Number of weights` = n())}) %>%
    left_join(data.frame(MONTH = 1:12, 
                         month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"),
                                                levels = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"))
    )) %>%
    select(-MONTH)
  
  wgts2tile <- expand.grid(YEAR = specs$years, 
                           month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                             "Apr", "May", "Jun",
                                                             "Jul", "Aug", "Sep",
                                                             "Oct", "Nov", "Dec"),
                                                  levels = c("Jan", "Feb", "Mar",
                                                             "Apr", "May", "Jun",
                                                             "Jul", "Aug", "Sep",
                                                             "Oct", "Nov", "Dec")),
                           data_source = unique(wgts2tile0$data_source)) %>%
    left_join(wgts2tile0)
  
  p_number_weights <-
    ggplot(wgts2tile,
           aes(x = YEAR, y = month_display, fill = `Number of weights`)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "white", high = "red") +
    scale_y_discrete(limits = rev) +
    coord_fixed() +
    ggtitle(paste0("Number of weights by data source"),
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label))) +
    geom_text(aes(label = `Number of weights`), color = "black", size = 2) +
    facet_wrap(~data_source) +
    theme_bw() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none")
  
  print(p_number_weights)
  
  ## Plot number of lengths ######################################################
  
  lengths2tile0 <-
    dat$bsm_dat %>%
    group_by(YEAR, MONTH) %>%
    summarise(`Number of lengths` = sum(NUMLEN)) %>%
    mutate(data_source = "Port aggregate") %>% # Updated label
    bind_rows({len2hist %>%
        filter(data_source %in% c("Survey", "MADMF BTS", "Port individual")) %>% # Updated filter
        group_by(YEAR, MONTH, data_source) %>%
        summarise(`Number of lengths` = n())}) %>%
    left_join(data.frame(MONTH = 1:12, 
                         month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"),
                                                levels = c("Jan", "Feb", "Mar",
                                                           "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep",
                                                           "Oct", "Nov", "Dec"))
    )) %>%
    select(-MONTH)
  
  lengths2tile <- expand.grid(YEAR = specs$years, 
                              month_display = factor(x      = c("Jan", "Feb", "Mar",
                                                                "Apr", "May", "Jun",
                                                                "Jul", "Aug", "Sep",
                                                                "Oct", "Nov", "Dec"),
                                                     levels = c("Jan", "Feb", "Mar",
                                                                "Apr", "May", "Jun",
                                                                "Jul", "Aug", "Sep",
                                                                "Oct", "Nov", "Dec")),
                              data_source = unique(lengths2tile0$data_source)) %>%
    left_join(lengths2tile0)
  
  p_number_lengths <-
    ggplot(lengths2tile,
           aes(x = YEAR, y = month_display, fill = `Number of lengths`)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "white", high = "red") +
    scale_y_discrete(limits = rev) +
    coord_fixed() +
    ggtitle(paste0("Number of lengths by data source"),
            subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                              unique(dat$srv_dat$species_label))) +
    geom_text(aes(label = `Number of lengths`), color = "black", size = 2) +
    facet_wrap(~data_source) +
    theme_bw() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none")
  
  print(p_number_lengths)
  
  
  ## Plot spatial distribution of NMFS BTS length-weight samples #################
  
  # Initialize as NULL so the ggsave logic at the end doesn't fail
  p_spatial_bts <- NULL 
  
  # Trap: Only proceed if srv_dat exists
  if (!is.null(dat$srv_dat)) {
    
    # Use exact NEFSC column names for coordinates and tow ID
    if (all(c("DECDEG_BEGLAT", "DECDEG_BEGLON", "ID") %in% names(dat$srv_dat))) {
      
      bts_spatial <- dat$srv_dat %>%
        filter(PURPOSE_CODE == 10) %>% # NMFS BTS
        # Group by unique tow ID and coordinates to count length-weight observations
        group_by(ID, DECDEG_BEGLAT, DECDEG_BEGLON) %>%
        summarise(n_lw_samples = n(), .groups = "drop") %>%
        # Filter out missing coordinates just in case
        filter(!is.na(DECDEG_BEGLAT) & !is.na(DECDEG_BEGLON))
      
      # Pull state borders for a simple geographic background
      coast_map <- map_data("state")
      
      p_spatial_bts <-
        ggplot() +
        # Draw the coastline
        geom_polygon(data = coast_map, aes(x = long, y = lat, group = group),
                     fill = "grey90", color = "white") +
        # Plot the survey tows
        geom_point(data = bts_spatial,
                   aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, size = n_lw_samples),
                   alpha = 0.3) +
        # Dynamically zoom the map based on the data footprint
        coord_quickmap(xlim = range(bts_spatial$DECDEG_BEGLON, na.rm = TRUE) + c(-1, 1),
                       ylim = range(bts_spatial$DECDEG_BEGLAT, na.rm = TRUE) + c(-1, 1)) +
        theme_bw() +
        labs(
          title = "Spatial Distribution of NMFS BTS Samples",
          subtitle = paste0(unique(dat$srv_dat$stock_label), " ",
                            unique(dat$srv_dat$species_label), " | All years combined"),
          x = "Longitude",
          y = "Latitude",
          size = "Length-weight \nsamples\nper tow"
        )
      
      print(p_spatial_bts)
      
    } else {
      warning("Spatial plot skipped: 'DECDEG_BEGLAT', 'DECDEG_BEGLON', or 'ID' not found in dat$srv_dat.")
    }
    
  } else {
    message("Spatial plot skipped: 'srv_dat' not found in the dataset.")
  }
  
  if(save_plots) {
    if(!dir.exists("plots")) dir.create("plots")
    
    # Extract labels to use cleanly in the filenames
    stock_name <- unique(dat$srv_dat$stock_label)
    spp_name   <- unique(dat$srv_dat$species_label)
    
    ggsave(paste0("plots/raw_lengths_hist_", stock_name, "_", spp_name, ".png"), p_hist, w = 7, h = 6)
    ggsave(paste0("plots/raw_lengths_dens_", stock_name, "_", spp_name, ".png"), p_dens, w = 7, h = 6)
    ggsave(paste0("plots/raw_lengths_hist_month_", stock_name, "_", spp_name, ".png"), p_hist_month, w = 7, h = 6)
    ggsave(paste0("plots/raw_lengths_dens_month_", stock_name, "_", spp_name, ".png"), p_dens_month, w = 7, h = 6)
    ggsave(paste0("plots/raw_port_total_weights_", stock_name, "_", spp_name, ".png"), p_port_total_weights, w = 12, h = 6)
    ggsave(paste0("plots/raw_number_of_weights_", stock_name, "_", spp_name, ".png"), p_number_weights, w = 9, h = 7)
    ggsave(paste0("plots/raw_number_of_lengths_", stock_name, "_", spp_name, ".png"), p_number_lengths, w = 9, h = 7)
    
    # Safely check if p_spatial_bts is not null before saving
    if(!is.null(p_spatial_bts)) {
      ggsave(paste0("plots/spatial_bts_data_", stock_name, "_", spp_name, ".png"), p_spatial_bts, w = 9, h = 9)  
    }
    
  }
  
}