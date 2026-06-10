## Plot cleaned data in various ways ###########################################
plot_cleaned_data <- function(dat1, dat2 = NULL, save_plots = FALSE) {
  
  # --- Combine the two datasets if dat2 is provided ---
  if (!is.null(dat2)) {
    dat <- dplyr::bind_rows(dat1, dat2)
  } else {
    dat <- dat1
  }
  
  # --- Create a combined Stock & Species label for faceting ---
  dat <- dat %>%
    mutate(stock_spp_label = paste(stock_label, species_label, sep = " "))
  
  # --- Dynamically create subtitle with specific date ranges for each stock ---
  subtitle_info <- dat %>%
    group_by(stock_spp_label) %>%
    summarise(
      min_yr = min(YEAR, na.rm = TRUE),
      max_yr = max(YEAR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label_with_years = paste0(stock_spp_label, " (", min_yr, "-", max_yr, ")"))
  
  sub_text <- paste(subtitle_info$label_with_years, collapse = " & ")
  
  # --- Safely collapse for filenames ---
  file_label <- paste(gsub(" ", "_", unique(dat$stock_spp_label)), collapse = "_and_")
  
  # --- Update data source labels & add month factors for all plots ---
  dat <- dat %>%
    mutate(
      data_source = case_when(
        data_source == "NMFS BTS" ~ "Survey",
        data_source == "Port"     ~ "Port aggregate",
        data_source == "Port DMF" ~ "Port individual",
        TRUE                      ~ data_source # Keeps any other names unchanged
      ),
      # Add standard month display factors natively for faceting
      month_display = factor(month.abb[MONTH], 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    )
  
  ## Plot gutted vs whole LW ###################################################
  p_gut_vs_whole <-
    ggplot(dat %>% 
             filter(data_source != "Port aggregate") %>%
             mutate(gutted_label = ifelse(ind_gutted, "Gutted", "Whole")), 
           aes(x = Ltru_i, y = Wobs_s, color = gutted_label, shape = data_source)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~stock_spp_label) + 
    theme_bw() +
    xlab("Length (cm)") +
    ylab("Weight (kg)") +
    ggtitle("Individual length-weight data", subtitle = sub_text) +
    labs(color = NULL) +
    theme(legend.title = element_blank())
  
  print(p_gut_vs_whole)
  
  
  ## Plot length distributions (Histograms & Densities) ########################
  
  # Over all months: Histogram
  p_hist <- 
    ggplot(dat, aes(x = Ltru_i, color = data_source, fill = data_source)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    facet_wrap(~stock_spp_label) +
    theme_bw() +
    xlab("Length (cm)") +
    ggtitle("Histogram of lengths by source", subtitle = sub_text) +
    theme(legend.title = element_blank())
  
  print(p_hist)
  
  # Over all months: Density
  p_dens <- 
    ggplot(dat, aes(x = Ltru_i, color = data_source, fill = data_source)) +
    geom_density(alpha = 0.5, position = "identity") +
    facet_wrap(~stock_spp_label) +
    theme_bw() +
    xlab("Length (cm)") +
    ggtitle("Probability density of lengths by source") +
    theme(legend.title = element_blank())
  
  print(p_dens)
  
  # By month: Histogram (Matrix layout)
  p_hist_month <-
    ggplot(dat, aes(x = Ltru_i, color = data_source, fill = data_source)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    facet_grid(stock_spp_label ~ month_display) +
    theme_bw() +
    xlab("Length (cm)") +
    ggtitle("Histogram of lengths by source", subtitle = sub_text) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_hist_month)
  
  # By month: Density (Matrix layout)
  p_dens_month <- 
    ggplot(dat, aes(x = Ltru_i, color = data_source, fill = data_source)) +
    geom_density(alpha = 0.5, position = "identity") +
    facet_grid(stock_spp_label ~ month_display) +
    theme_bw() +
    xlab("Length (cm)") +
    ggtitle("Probability density of lengths by source", subtitle = sub_text) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_dens_month)
  
  
  ## Plot number of weights by data source #####################################
  # 1. Summarize including the combined stock_spp_label
  heatmap_dat <- dat %>%
    group_by(stock_spp_label, YEAR, month_display, data_source) %>% 
    summarise(
      `Number of lengths` = n(),
      `Number of weights` = n_distinct(s),
      .groups = "drop"
    )
  
  # 2. Expand grid to include all combinations natively
  heatmap_grid <- heatmap_dat %>%
    tidyr::complete(stock_spp_label,
                    YEAR = seq(min(dat$YEAR, na.rm = TRUE), max(dat$YEAR, na.rm = TRUE)), 
                    month_display, 
                    data_source)
  
  # ==============================================================================
  # Plot: Number of Weights (Log-Transformed Bubble Chart)
  # ==============================================================================
  p_number_weights <-
    ggplot(heatmap_grid %>% filter(!is.na(`Number of weights`)), 
           aes(x = YEAR, y = month_display, 
               size = `Number of weights`, color = `Number of weights`)) +
    geom_point(alpha = 0.8) +
    scale_size_continuous(range = c(2, 9), trans = "log10", 
                          breaks = c(1, 10, 50, 100, 500, 1000, 2500, 5000, 10000),
                          guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_color_gradient(low = "lightblue", high = "darkblue", trans = "log10",
                         breaks = c(1, 10, 50, 100, 500, 1000, 2500, 5000, 10000),
                         guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_y_discrete(limits = rev) +
    ggtitle("Number of weights by data source") +
    facet_grid(data_source ~ stock_spp_label) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
  
  print(p_number_weights)
  
  # ==============================================================================
  # Plot: Number of Lengths (Linear Bubble Chart)
  # ==============================================================================
  p_number_lengths <-
    ggplot(heatmap_grid %>% filter(!is.na(`Number of lengths`)), 
           aes(x = YEAR, y = month_display, 
               size = `Number of lengths`, color = `Number of lengths`)) +
    geom_point(alpha = 0.8) +
    scale_size_continuous(range = c(1, 9), breaks = function(x) pretty(x, n = 6),
                          guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_color_gradient(low = "lightcoral", high = "darkred", breaks = function(x) pretty(x, n = 6),
                         guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_y_discrete(limits = rev) +
    ggtitle("Number of lengths by data source") +
    facet_grid(data_source ~ stock_spp_label) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
  
  print(p_number_lengths)
  
  # ==============================================================================
  # Plot: Number of fish per Port aggregate sample
  # ==============================================================================
  port_agg_samples <- dat %>%
    filter(data_source == "Port aggregate") %>%
    group_by(stock_spp_label, s) %>%
    summarise(n_fish = n(), .groups = "drop")
  
  p_fish_per_sample <-
    ggplot(port_agg_samples, aes(x = n_fish)) +
    geom_histogram(fill = "steelblue", color = "white", binwidth = 5, boundary = 0) +
    facet_wrap(~stock_spp_label) +
    theme_bw() +
    ggtitle("Number of fish per Port aggregate sample", subtitle = sub_text) +
    xlab("Number of fish in sample") +
    ylab("Frequency (Number of samples)")
  
  print(p_fish_per_sample)
  
  
  if(save_plots) {
    if(!dir.exists("plots")) dir.create("plots")
    
    ggsave(paste0("plots/cleaned_gut_vs_whole_", file_label, ".jpg"), 
           p_gut_vs_whole, w = 10, h = 6, dpi = 300)
    
    ggsave(paste0("plots/cleaned_lengths_hist_", file_label, ".jpg"), 
           p_hist, w = 10, h = 6, dpi = 300)
    
    ggsave(paste0("plots/cleaned_lengths_dens_", file_label, ".jpg"), 
           p_dens, w = 10, h = 6, dpi = 300)
    
    ggsave(paste0("plots/cleaned_lengths_hist_month_", file_label, ".jpg"), 
           p_hist_month, w = 14, h = 8, dpi = 300)
    
    ggsave(paste0("plots/cleaned_lengths_dens_month_", file_label, ".jpg"), 
           p_dens_month, w = 14, h = 8, dpi = 300)
    
    ggsave(paste0("plots/cleaned_number_of_weights_", file_label, ".jpg"),
           p_number_weights, w = 14, h = 10, dpi = 300)
    
    ggsave(paste0("plots/cleaned_number_of_lengths_", file_label, ".jpg"), 
           p_number_lengths, w = 14, h = 10, dpi = 300)
    
    ggsave(paste0("plots/cleaned_fish_per_port_agg_sample_", file_label, ".jpg"), 
           p_fish_per_sample, w = 10, h = 6, dpi = 300)
  }
  
}