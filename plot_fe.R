plot_fe <- function(mods, dat = NULL, make_table = FALSE) {
  
  # --- Input Validation ---
  if(is.null(dat)) {
    stop("Argument 'dat' must be provided.")
  }
  
  # --- Standardize data source labels in raw data ---
  dat <- dat %>%
    mutate(data_source = case_when(
      data_source == "NMFS BTS" ~ "Survey",
      data_source == "Port"     ~ "Port aggregate",
      data_source == "Port DMF" ~ "Port individual",
      TRUE                      ~ data_source
    ))
  
  ## Data Preparation ####
  # This section extracts fixed effect parameters from the model object
  # and structures them into a tidy data frame for plotting.
  ests <- purrr::map_dfr(mods, pull_fe) %>%
    mutate(
      # 1. Update NMFS BTS
      variable = gsub("NMFS BTS", "Survey", variable),
      Model    = gsub("NMFS BTS", "Survey", Model),
      
      # 2. Update Port DMF
      variable = gsub("Port DMF", "Port individual", variable),
      Model    = gsub("Port DMF", "Port individual", Model),
      
      # 3. Update remaining standalone "Port" 
      # (Negative lookahead prevents altering "Port individual")
      variable = gsub("Port(?! individual)", "Port aggregate", variable, perl = TRUE),
      Model    = gsub("Port(?! individual)", "Port aggregate", Model, perl = TRUE)
    )
  
  
  ## Plotting ####
  if(length(unique(ests$Model)) == 1) {
    for (i in unique(ests$param_type)) {
      p <-
        ggplot(ests %>% filter(param_type == i), aes(x = variable, y = value)) +
        geom_point() +
        geom_errorbar(aes(ymin = CI_90lo, ymax = CI_90hi), width = 0.1) +
        theme_bw() +
        ggtitle(paste0("Parameter estimates (", i, ")"),
                subtitle = paste0(unique(dat$stock_label), " ",
                                  unique(dat$species_label))) +
        labs(caption = "The vertical bars are the 90% confidence intervals.") +
        ylab("") +
        xlab("") +
        theme(text = element_text(size = 14))
      
      if(!dir.exists("./plots")) dir.create("./plots")
      ggsave(plot = p, paste0("./plots/param_ests_", i, ".jpg"), w = 8, h = 7)
      
      print(p)
    } 
  }
  
  if(length(unique(ests$Model)) > 1) {
    for (i in unique(ests$param_type)) {
      pd <- position_dodge(width = 0.5)
      p <-
        ggplot(ests %>% filter(param_type == i), aes(x = variable, y = value, 
                                                     color = Model)) +
        geom_point(position = pd) +
        geom_errorbar(aes(ymin = CI_90lo, ymax = CI_90hi), width = 0.05,
                      position = pd) +
        theme_bw() +
        ggtitle(paste0("Parameter estimates (", i, ")"),
                subtitle = paste0(unique(dat$stock_label), " ",
                                  unique(dat$species_label))) +
        labs(caption = "The vertical bars are the 90% confidence intervals.") +
        ylab("") +
        xlab("") +
        theme(text = element_text(size = 14))
      
      if(!dir.exists("./plots")) dir.create("./plots")
      ggsave(plot = p, paste0("./plots/fe_estimates_", i, ".jpg"), w = 8, h = 7)
      
      print(p)
    } 
    
  }
  
  ## Make Table if requested ####
  if (make_table) {
    
    if(!dir.exists("./tables")) dir.create("./tables")
    
    table_fe <- 
      ests %>% 
      select(-param_type) %>%
      rename(Parameter = variable,
             `90% CI lower` = CI_90lo,
             Estimate = value,
             `90% CI upper` = CI_90hi) %>%
      select(Model, Parameter,  Estimate, `90% CI lower`, `90% CI upper`)
    
    mean_gutted_whole <-
      table_fe %>%
      filter(Parameter == "whole-to-gutted multiplier") %>%
      summarise(`Gutted to whole mean across models` = 1/mean(Estimate)) %>%
      print()
    
    table_pretty <-
      pander::pandoc.table(table_fe,
                           split.table = Inf,
                           caption = "Fixed effects estimates and 90% confidence interval."
      )
    
    writexl::write_xlsx(table_fe,
                        paste0("./tables/table_fe_", unique(dat$stock_label), 
                               "_", unique(dat$species_label), ".xlsx"))
    
  }
  
}