# function to set up crossvalidation
set_up_crossval <- function(mods, dat, k, kfold, ts, max_years_ahead,
                            lodo, lodi, custom, custom_test_train) {
  
  # Do some checks on the inputs
  if(ts & k >= length(min(unique(dat$YEAR)):max(unique(dat$YEAR)))) {
    stop(paste0("For timeseries cross-validation, the number of years in the ",
    "test set (k) cannot match or exceed the range of years in the data set. ",
    "Here, k is set to ", k, " and the range of years is ",
    length(min(unique(dat$YEAR)):max(unique(dat$YEAR))), " (",
    min(unique(dat$YEAR)) , " - ", max(unique(dat$YEAR)), ")."))
  }
  
  if(ts & (is.null(max_years_ahead) | !is.null(max_years_ahead) && max_years_ahead <= 0)) {
    stop(paste0("For timeseries cross-validation (ts = TRUE), max_years_ahead ",
    "must be set to ≥ 1."))
  }
  
  # converged_models <- list()
  # c <- 1
  # for(i in 1:length(mods)) {
  #   if(mods[[i]]$opt$convergence == 0) {
  #     converged_models[[c]] <- mods[[i]]$model_effects
  #     c <- c + 1
  #   }
  # }
  
  
  test_list <- list()
  if(ts) { # ts cross-validation
    
    for(i in 1:k) {
      test_yw_ind <- which(dat$YW %in% seq(max(dat$YW)-365*i + 1, 
                                           max(dat$YW), 
                                           "days"))
      test_list[[i]] <- data.frame(s_test = unique(dat$s[test_yw_ind]),
                                   fold   = i,
                                   test_type =paste0("Time series cross-validation ",
                                                     k, "-year peel ", "(1 to ",
                                                     max_years_ahead, " years ahead)"))
    }
  } else if(lodo) { # leave-one-dataset out cross-validation
    
    # k is set to the number of datasets if using lodo
    unique_data_sources <- unique(dat$data_source)
    k <- length(unique_data_sources)
    for(i in 1:k) {
      ind_data_source <- which(dat$data_source == unique_data_sources[i])
      test_list[[i]] <- data.frame(s_test = unique(dat$s[ind_data_source]),
                                   fold = i,
                                   test_type = paste0("Leave-one-dataset-out cross-validation"))
    }
  } else if(lodi) { # leave-one-dataset in cross-validation
    
    # k is set to the number of datasets if using lodi
    unique_data_sources <- unique(dat$data_source)
    k <- length(unique_data_sources)
    for(i in 1:k) {
      ind_data_sources <- which(dat$data_source != unique_data_sources[i])
      test_list[[i]] <- data.frame(s_test = unique(dat$s[ind_data_sources]),
                                   fold = i,
                                   test_type = paste0("Leave-one-dataset-in cross-validation"))
    }
  } else if(kfold) { # standard k-fold cross-validation
    s_rand     <- sample(unique(dat$s))
    size_test  <- floor(length(s_rand) / k)
    remainder  <- length(s_rand) %% k
    
    ind_test_fold <- c(rep(1:k, each = size_test), rep(k, remainder))
    
    for(i in 1:k) {
      test_list[[i]]  <- data.frame(s_test = s_rand[ind_test_fold == i],
                                    fold = i,
                                    test_type =paste0(k, "-fold cross-validation"))
    }
  } else if(custom){
    k <- length(custom_test_train) 
    
    for(i in 1:k) { 
      # Extract the IDs the user wants to use for this fold
      s_test  <- custom_test_train[[i]]$test
      s_train <- custom_test_train[[i]]$train
      
      # Validation: Ensure the provided test IDs exist in the data
      if(!any(s_test %in% dat$s)) {
        stop(paste0("Fold ", i, ": None of the provided test IDs exist in dat$s."))
      }
      
      # Optional Validation: Ensure train IDs exist
      if(!is.null(s_train) && !any(s_train %in% dat$s)) {
        stop(paste0("Fold ", i, ": None of the provided training IDs exist in dat$s."))
      }
      
      # Create the data frame for this fold
      # We use list columns or repeat the fold/type info to keep it tidy
      
      test_list[[i]] <- data.frame(
        s_test    = unique(s_test),
        fold      = names(custom_test_train[i]),
        test_type = "User-provided custom test and train set"
      )
      
      # Storing s_train as an attribute or a separate column if lengths match.
      # Best practice for varying lengths is to store as a list attribute:
      attr(test_list[[i]], "s_train") <- unique(s_train)
    }
  } else {
    stop("No cross-validation method was specified.")
  }
  
  
  # create list of all models and test sets
  models_and_test_list <- vector("list", length(mods)*k)
  c <- 1
  for(i in 1:length(mods)) {
    for(j in 1:k) {
      models_and_test_list[[c]]$model_name <- mods[[i]]$model_name
      models_and_test_list[[c]]$model_effects <- mods[[i]]$model_effects
      models_and_test_list[[c]]$test <- test_list[[j]]
      # Use mapped parameters if used in the original fit
      models_and_test_list[[c]]$map <- mods[[i]]$obj$env$map
      full_pars <- mods[[i]]$obj$env$parList()
      mapped_names <- names(models_and_test_list[[c]]$map)
      models_and_test_list[[c]]$inits <- full_pars[names(full_pars) %in% mapped_names]
      
      
      # For some forms of cross-validation we need to set some parameters to
      # pre-estimated values because 
      # they can be inestimable when entire datasets are held out. If more
      # datasets are added in the future it is possible that some of these 
      # parameters could be estimable and in that case we should edit 
      # the below to allow for estimation.
      if(lodo || (any("log_g" %in% names(mods[[i]]$opt$par)) && 
         !is.null(attr(test_list[[j]], "s_train")) && 
         length(unique(dat$ind_gutted[dat$s %in% attr(test_list[[j]], "s_train")])) < 2)) {
        if(is.null(models_and_test_list[[c]]$inits)) models_and_test_list[[c]]$inits <- list()
        if(is.null(models_and_test_list[[c]]$map)) models_and_test_list[[c]]$map   <- list()
        # need to map gutting effect if using because it can't be estimated
        # when all data in a train set is just gutted or just whole
        if(any("log_g" %in% names(mods[[i]]$opt$par))) {
          message(paste0("log_g and log_a fixed at previously estimated values becuase the",
          " train set does not contain both gutted and whole fish, or lodo was requested."))
          models_and_test_list[[c]]$inits$log_g <- mods[[i]]$opt$par["log_g"]
          models_and_test_list[[c]]$map$log_g   <- factor(NA)
          models_and_test_list[[c]]$inits$log_a <- mods[[i]]$opt$par["log_a"]
          models_and_test_list[[c]]$map$log_a   <- factor(NA)
        }
      }
        # need to map dataset sds if some data sources are held out of the train set
        if(!is.null(attr(test_list[[j]], "s_train")) && 
           length(unique(dat$data_source[dat$s %in% attr(test_list[[j]], "s_train")])) != 
           sum(names(mods[[i]]$opt$par) == "log_sd_s")){
          message(paste0("log_sd_s fixed at previously estimated values becuase the",
                         " train set does not contain all data sources."))
          models_and_test_list[[c]]$inits$log_sd_s <- 
            mods[[i]]$opt$par[which(names(mods[[i]]$opt$par) == "log_sd_s")]
          models_and_test_list[[c]]$map$log_sd_s <- 
            rep(factor(NA), length(which(names(mods[[i]]$opt$par) == "log_sd_s")))
        }
      
      c <- c + 1
    }
  }
  
  return(models_and_test_list)
}

