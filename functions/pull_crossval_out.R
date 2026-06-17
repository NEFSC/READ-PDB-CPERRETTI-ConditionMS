pull_crossval_out <- function(mod_crossval, dat) {
    
  
  if(grepl("Time series", unique(mod_crossval$test$test_type))){
    is_tscv <- TRUE
    max_years_ahead <- 
      stringr::str_extract(unique(mod_crossval$test$test_type), "(?<=to )\\d+") %>%
      as.numeric()
  } else {
    is_tscv <- FALSE
    max_years_ahead <- NA
  }
  
  data.frame(s = mod_crossval$rep$unique_test_s,
             fold = mod_crossval$test$fold,
             test_type = mod_crossval$test$test_type) %>%
    mutate(model_name = mod_crossval$model_name, 
           name_and_effects = paste0(mod_crossval$model_name, ": ", 
                                      toString(mod_crossval$model_effects)),
           nlminb_converged = (mod_crossval$opt$convergence == 0),
           pred_Wobs_s_test = mod_crossval$rep$pred_Wobs_s_test,
           is_tscv = is_tscv,
           max_years_ahead = max_years_ahead) 
  
  
}