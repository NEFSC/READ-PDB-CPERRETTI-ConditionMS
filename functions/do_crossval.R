do_crossval <- function(mods, dat, k = 10, kfold = FALSE, ts = FALSE, max_years_ahead = 1,
                        lodo = FALSE, lodi = FALSE, custom = FALSE, custom_test_train = NULL,
                        do_rep = TRUE, do_sdrep = FALSE,
                        reduce_output = TRUE) {
  
  if(sum(c(kfold, ts, lodo, lodi, custom)) > 1) { 
  stop("Only one of 'kfold', 'ts', 'lodo', 'lodi', or 'custom' can be set to true.") 
  }
  
  if(sum(c(kfold, ts, lodo, lodi, custom)) == 0) { 
    stop("One of 'kfold', 'ts', 'lodo', 'lodi', or 'custom' must be set to true.") 
  }
  
  models_and_test_list <- set_up_crossval(mods = mods, 
                                          dat = dat,
                                          k = k,
                                          kfold = kfold,
                                          ts = ts,
                                          max_years_ahead = max_years_ahead,
                                          lodo = lodo,
                                          lodi = lodi,
                                          custom = custom,
                                          custom_test_train = custom_test_train
                                          )
  
  
  
  if(ts) message("Running time series cross-validation")
  if(lodo) message("Running leave-one-dataset-out cross-validation")
  if(lodi) message("Running leave-one-dataset-in cross-validation")
  if(custom) message("Running custom cross-validation")
  if(kfold) message("Running k-fold cross-validation")
  
  plan(multisession, workers = availableCores() - 1)
  tictoc::tic()
  mods_crossval <- future_map(models_and_test_list,
                              ~fit_lw(dat = dat,
                                      model_effects = .$model_effects,
                                      model_name = .$model_name,
                                      inits = .$inits,
                                      map = .$map,
                                      do_rep = do_rep,
                                      do_sdrep = do_sdrep,
                                      check_estim = FALSE,
                                      silent = TRUE,
                                      test = .$test,
                                      reduce_output = reduce_output),
                              .options = furrr_options(seed = TRUE))
  tictoc::toc()
  plan(sequential)
  
  return(mods_crossval)
}

