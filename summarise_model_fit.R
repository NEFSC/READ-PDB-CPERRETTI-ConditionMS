summarise_model_fit <- function(mods, dat) {
  # Required for professional table rendering
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is required for bolding. Please install it.")
  }
  library(kableExtra)
  
  # 1. Pre-aggregate observed data template
  obs_template <- dat %>%
    group_by(data_source, s) %>%
    summarise(Wobs_s = first(Wobs_s), .groups = "drop")
  
  bias_list <- list()
  msa_list  <- list()
  
  for(i in seq_along(mods)) {
    mod_obj  <- mods[[i]]
    mod_name <- names(mods)[i]
    if(is.null(mod_name) || mod_name == "") mod_name <- paste0("Model_", i)
    
    # Aggregate predictions
    mod_metrics <- dat %>%
      ungroup() %>%
      mutate(W_i_det = exp(mod_obj$rep$log_W_i_det)) %>%
      group_by(data_source, s) %>%
      summarise(Wpred_s = sum(W_i_det), .groups = "drop") %>%
      left_join(obs_template, by = c("data_source", "s")) %>%
      mutate(log_ratio = log(Wpred_s) - log(Wobs_s))
    
    # MSSPB (Bias)
    bias_list[[i]] <- mod_metrics %>%
      mutate(SSPB = sign(log_ratio) * 100 * (exp(abs(log_ratio)) - 1)) %>%
      group_by(data_source) %>%
      summarise(MetricValue = mean(SSPB, na.rm = TRUE), .groups = "drop") %>%
      mutate(Model = mod_name)
    
    # MSA (Accuracy)
    msa_list[[i]] <- mod_metrics %>%
      group_by(data_source) %>%
      summarise(MetricValue = 100 * (exp(median(abs(log_ratio), na.rm = TRUE)) - 1), 
                .groups = "drop") %>%
      mutate(Model = mod_name)
  }
  
  # 2. Pivot to Wide (Models as Rows, Sources as Columns)
  format_table <- function(results_list) {
    bind_rows(results_list) %>%
      tidyr::pivot_wider(names_from = data_source, values_from = MetricValue)
  }
  
  df_bias <- format_table(bias_list)
  df_msa  <- format_table(msa_list)
  
  # 3. Helper to Render the Table with kableExtra
  render_performance_table <- function(df, title, type = "bias") {
    source_cols <- names(df)[names(df) != "Model"]
    
    # Initialize the kable object
    k_tab <- kable(df, digits = 2, caption = title, format = "simple") 
    
    # For each source column, find the best row and apply styling
    # Note: 'simple' format is best for console, but 'html' or 'latex' 
    # is required for actual bolding in RStudio/Markdown.
    
    # If we are in an interactive session, let's use a cleaner display
    df_styled <- df
    for(col in source_cols) {
      vals <- as.numeric(df[[col]])
      best_row <- if(type == "bias") which.min(abs(vals)) else which.min(vals)
      
      # We format the values to 2 decimals
      df_styled[[col]] <- sprintf("%.2f", df[[col]])
      # Use kableExtra's cell_spec for actual bolding (works in HTML/Latex)
      df_styled[[col]][best_row] <- cell_spec(df_styled[[col]][best_row], bold = TRUE)
    }
    
    k_tab <- kbl(df_styled, escape = FALSE, caption = title) %>%
      kable_classic(full_width = FALSE, html_font = "Arial") %>%
      row_spec(0, bold = TRUE) # Bold headers
    
    return(k_tab)
  }
  
  # 4. Generate Tables
  bias_table <- render_performance_table(df_bias, "Table 1: Fit Bias (SSPB %)", "bias")
  msa_table  <- render_performance_table(df_msa, "Table 2: Fit Accuracy (MSA %)", "msa")
  
  # Print to viewer
  print(bias_table)
  print(msa_table)
  
  return(invisible(list(bias = df_bias, msa = df_msa)))
}