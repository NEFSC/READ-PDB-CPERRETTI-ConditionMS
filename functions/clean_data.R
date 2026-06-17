# Clean and organize data for model ############################################
clean_data <- function(dat) {
  
  
  if("bsm_dat" %in% names(dat)) {
    bsm_dat <-
      dat$bsm_dat %>%
      filter(# remove potential data errors:
        !(WGTSAMP %in% c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000,
                         1100, 1200))) %>% # << Update in future to include more 100s
      group_by(LINK, TALLYNO, SPPLNDLB, MONTH, DAY, WGTSAMP) %>%
      mutate(Ltru_i = as.numeric(LENGTH),
             Wobs_s = as.numeric(WGTSAMP_KG),
             DATE = paste(YEAR, MONTH, DAY, sep = "-"),
             WEEK = strftime(DATE, format = "%V"),
             data_source = "Port", # Kept original
             s0 = as.numeric(cur_group_id())) %>%
      rename(LOC = AREA) %>%
      uncount(NUMLEN)
  }
  
  if("dmf_dat" %in% names(dat)) {
    dmf_dat <-
      dat$dmf_dat %>%
      select(species_label, stock_label,
             TALLY_NO, SAMPLING_DATE, SAMPLING_YR, MONTH,
             SPECIES_ITIS, COMMON_NAME, MARKET_DESC, ORGANISM_ID, TALLY_VESSEL_SEQ,
             SAMPLE_SEQ, GRADE_DESC,
             UNIT_MEASURE, PARAM_DESCR, PARAM_VALUE_NUM, AREA_CODE) %>%
      filter(PARAM_DESCR %in% c("FORK LENGTH", "TOTAL LENGTH", "ORGANISM WEIGHT"),
             !(SPECIES_ITIS == 164727 & 
                 PARAM_DESCR == 	"FORK LENGTH" & 
                 PARAM_VALUE_NUM > 750) # Remove two obvious outliers for pollock
      ) %>%
      mutate(# Convert lengths to CM and weights to KG:
        PARAM_DESCR = ifelse(PARAM_DESCR %in% c("FORK LENGTH", "TOTAL LENGTH"), "length", PARAM_DESCR),
        PARAM_VALUE_NUM_new = PARAM_VALUE_NUM,
        PARAM_VALUE_NUM_new = ifelse(UNIT_MEASURE == "MM", PARAM_VALUE_NUM_new/10, PARAM_VALUE_NUM_new),
        PARAM_VALUE_NUM_new = ifelse(UNIT_MEASURE == "GM", PARAM_VALUE_NUM_new/1000, PARAM_VALUE_NUM_new),
        PARAM_VALUE_NUM_new = ifelse(UNIT_MEASURE == "LB", PARAM_VALUE_NUM_new*0.453592, PARAM_VALUE_NUM_new),
        UNIT_MEASURE_new = ifelse(UNIT_MEASURE == "MM", "CM", UNIT_MEASURE),
        UNIT_MEASURE_new = ifelse(UNIT_MEASURE == "GM", "KG", UNIT_MEASURE_new),
        UNIT_MEASURE_new = ifelse(UNIT_MEASURE == "LB", "KG", UNIT_MEASURE_new)) %>%
      select(-UNIT_MEASURE, -UNIT_MEASURE_new, -PARAM_VALUE_NUM) %>%
      spread(PARAM_DESCR, PARAM_VALUE_NUM_new) %>%
      filter(!is.na(length),
             !is.na(`ORGANISM WEIGHT`)) %>%
      mutate(Ltru_i = as.numeric(length),
             Wobs_s = `ORGANISM WEIGHT`,
             YEAR = SAMPLING_YR,
             DATE   = substr(SAMPLING_DATE, 1, 10),
             WEEK = strftime(DATE, format = "%V"),
             LOC = AREA_CODE,
             data_source = "Port DMF") %>% # Kept original
      group_by(TALLY_NO, COMMON_NAME, MARKET_DESC, ORGANISM_ID, TALLY_VESSEL_SEQ,
               SAMPLE_SEQ) %>%
      mutate(s0 = as.numeric(cur_group_id())) %>%
      ungroup() %>%
      select(species_label, stock_label, 
             data_source, DATE, YEAR, MONTH, WEEK, LOC, s0, Ltru_i, Wobs_s) 
  }
  
  if("srv_dat" %in% names(dat)) {
    srv_dat <-
      dat$srv_dat %>%
      group_by(PURPOSE_CODE, CRUISE, YEAR, MONTH, EST_DAY, STRATUM, TOW,
               STATION, SVSPP, INDID, LENGTH) %>%
      mutate(s0 = as.numeric(cur_group_id()),
             data_source = ifelse(PURPOSE_CODE == 10, "NMFS BTS", NA), # Kept original
             data_source = ifelse(PURPOSE_CODE == 11, "MADMF BTS", data_source),
             DATE = paste(YEAR, MONTH, EST_DAY, sep = "-"),
             WEEK = strftime(END_EST_TOWDATE, format = "%V"),
             Ltru_i = LENGTH,
             Wobs_s = INDWT,
             YEAR = as.numeric(YEAR)) %>%
      rename(LOC = AREA) %>%
      ungroup() %>%
      select(species_label, stock_label,
             data_source, DATE, YEAR, MONTH, WEEK, LOC, s0, Ltru_i, Wobs_s) 
  }
  
  
  dat_woutliers <-
    data.frame(NULL) %>%
    {if("bsm_dat" %in% names(dat)) bind_rows(., bsm_dat) else .} %>%
    {if("srv_dat" %in% names(dat)) bind_rows(., srv_dat) else .} %>%
    {if("dmf_dat" %in% names(dat)) bind_rows(., dmf_dat) else .} %>%
    group_by(data_source, s0) %>%
    mutate(s = as.numeric(cur_group_id())) %>%
    ungroup() %>%
    mutate(ind_gutted = ifelse(data_source %in% c("Port", "Port DMF") &
                                 specs$itis %in% c(164744, 164712, 164727), 1, 0),
           WEEK = ifelse(WEEK == "53", "52", WEEK),
           WEEK = as.numeric(WEEK),
           YW_char = paste(YEAR, WEEK, "1", sep = "-"),
           YW   = as.Date(YW_char, format = "%Y-%U-%u"),
           SEMESTER = ifelse(lubridate::yday(DATE) < 183, 1, 2),
           QUARTER = lubridate::quarter(DATE),
    ) %>%
    select(species_label, stock_label, 
           data_source, YEAR, SEMESTER, QUARTER, MONTH, WEEK, YW, LOC, ind_gutted, s,
           Ltru_i, Wobs_s)
  
  # Remove suspected data errors
  #if("bsm_dat" %in% names(dat)) {
  
  simple_filter0 <-
    dat_woutliers %>%
    #filter(data_source == "Port") %>%
    group_by(data_source, s) %>%
    summarise(n_samp = n(),
              mean_length = mean(Ltru_i),
              mean_weight = unique(Wobs_s/n_samp),
              .groups = "drop") %>%
    filter(mean_length < 400) #(remove obvious outliers (4m fish)
  
  simple_lm <- lm(log(mean_weight)~log(mean_length), data = simple_filter0)
  
  simple_filter <- 
    simple_filter0 %>%
    left_join(data.frame(s= simple_filter0$s, 
                         exp(predict(simple_lm,interval = "prediction", 
                                     level = 0.9999, warning = FALSE) %>% 
                               suppressWarnings())), by = "s") %>%
    mutate(label = ifelse(#data_source == "Port" &
      (mean_weight < lwr | mean_weight > upr), 
      paste("remove", data_source),
      NA))
  
  percent_excluded <-
    simple_filter %>%
    distinct(s, label) %>%
    summarise(percent_excluded = 100 * sum(!is.na(label))/n()) %>%
    print()
  
  # Extract stock and species strings for titles and filenames
  stock_name <- unique(dat_woutliers$stock_label)[1]
  spp_name   <- unique(dat_woutliers$species_label)[1]
  
  # --- TEMPORARY DATAFRAME ONLY FOR PLOTTING ---
  # Change the labels dynamically just for the plot rendering
  plot_df <- simple_filter %>%
    mutate(label = case_when(
      label == "remove NMFS BTS" ~ "Remove Survey",
      label == "remove Port"     ~ "Remove Port aggregate",
      label == "remove Port DMF" ~ "Remove Port individual",
      TRUE                       ~ label
    ))
  
  # Determine non-NA labels present for legend breaks from the temporary df
  legend_breaks <- unique(plot_df$label[!is.na(plot_df$label)])
  
  p_filter <-
    ggplot(plot_df,
           aes(x = mean_length, y = mean_weight)) +
    geom_point(alpha = 0.7, aes(color = label, fill = label)) +
    geom_ribbon(aes(ymax = upr, ymin = lwr), alpha = 0.3, color = NA) +
    
    # Hide NA from legends but keep them visible on the plot as grey dots
    scale_color_discrete(breaks = legend_breaks, na.value = "grey75") + 
    scale_fill_discrete(breaks = legend_breaks, na.value = "grey75") +
    
    theme_minimal() +
    ggtitle("Outlier detection of samples",
            subtitle = paste0(stock_name, " ", spp_name)) +
    xlab("Mean length (cm)") +
    ylab("Mean weight (kg)") +
    # Drop the title "label"
    labs(color = NULL,
         fill = NULL, 
         caption = paste0("The shaded area is the 99.99% prediction interval.",
         "\nSamples with mean weights outside the prediction interval were removed."))
  
  print(p_filter)
  
  if(!dir.exists("plots")) dir.create("plots")
  ggsave(paste0("plots/filter_samples_", stock_name, "_", spp_name, ".png"),
         p_filter, w = 10, h = 8)
  #}
  
  
  
  
  
  dat <-
    dat_woutliers %>%
    #{if("bsm_dat" %in% names(dat))
    filter(., s %in% simple_filter$s[is.na(simple_filter$label)] #| 
           #data_source != "Port"
    ) %>% #else . } %>%
    group_by(data_source) %>%
    mutate(ind_data_source = cur_group_id())
  
  
  return(dat)
}
