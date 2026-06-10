pull_ests <- function(mod, dat, loc = NULL) {
  
  if(is.null(mod$pl)) {
    message(paste0("Cannot plot b because b is null. Make sure",
                   " the model was fit with do_sdrep = TRUE."))
    stop()
  }
  
  # --- STEP 1: Calculate fold_name safely outside the pipe ---
  f_name <- ""
  if ("fold" %in% names(mod$test)) {
    t_type <- unique(mod$test$test_type)
    
    if (grepl("series", t_type)) {
      # Math is only evaluated if it's actually a time-series fold
      f_name <- paste0(", leave out ", substr(max(dat$YW) - 365 * as.numeric(unique(mod$test$fold)) + 1, 1, 10), "+")
    } else if (grepl("eave-one-dataset-out", t_type)) {
      f_name <- paste0(", leave out ", unique(dat$data_source[dat$s %in% mod$test$s_test]))
    } else if (grepl("eave-one-dataset-in", t_type)) {
      f_name <- paste0(", leave in ", unique(dat$data_source[!(dat$s %in% mod$test$s_test)]))
    } else if (grepl("-fold", t_type)) {
      f_name <- paste0(", fold ", unique(mod$test$fold))
    } else {
      # Default for custom folds
      f_name <- paste0(", ", unique(mod$test$fold))
    }
  }
  
  # --- STEP 2: Use the pre-calculated f_name in the pipe ---
  ind_b <- mod$rep$ind_b
  if(!is.null(loc)) {
    ind_loc2use <- unique(mod$pred_specs$ind_loc_to_predict[mod$pred_specs$loc_to_predict == loc])
    ind_b <- ind_b %>% filter(ind_loc == ind_loc2use)
  }
  
  ests <-
    ind_b %>%
    left_join({mod$rep$ind_b %>%
        mutate(variable = "b",
               value = exp(mod$pl$log_b),
               CI_90lo = exp(mod$pl$log_b - 1.645*mod$plsd$log_b),
               CI_90hi = exp(mod$pl$log_b + 1.645*mod$plsd$log_b),
               fold_name = f_name, 
               model_fold = paste0(mod$model_name, ": ", 
                                   paste(mod$model_effects, collapse = " "), fold_name)
        )
    }
    ) %>%
    left_join(expand.grid(year_to_predict = unique(mod$pred_specs$year_to_predict),
                          week_to_predict = unique(mod$pred_specs$week_to_predict)) %>%
                left_join({mod$pred_specs %>% 
                    select(year_to_predict, week_to_predict, ind_yw_to_predict)}) %>%
                rename(YEAR = year_to_predict,
                       WEEK = week_to_predict,
                       ind_yw = ind_yw_to_predict) %>%
                mutate(YW_char = paste(YEAR, WEEK, "1", sep = "-"),
                       YW   = as.Date(YW_char, format = "%Y-%U-%u")),
              relationship = "many-to-many") %>%
    {
      # for time series cv only show predictions in the max_years_ahead interval
      if("test" %in% names(mod) && grepl("series", unique(mod$test$test_type))) {
        filter(., YW < (min(unique(dat$YW[dat$s %in% mod$test$s_test])) + 
                          lubridate::years(as.numeric(stringr::str_extract_all(mod$test$test_type,
                                                                               "\\d+")[[1]][3]))))  
      } else {. }
    }
}