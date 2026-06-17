# Model that fits a time-varying LW relationship incorporating sample and 
# individual weights
model <- function(parms, dat) {
  
  
  getAll(parms, dat, pred_specs, test, ind_b, ind_use, sdrep_pred)
  
  # Optional (enables extra RTMB features)
  #Wobs_s <- OBS(dat$Wobs_s)
  
  # Transform parameters
  sd_s <- exp(log_sd_s)
  
  # Initialize joint negative log likelihood
  nll <- 0
  
  ## Set up likelihood for log_b ###############################################
  if(exists("log_sd_d_w")) {
    sd_d_w <- exp(log_sd_d_w)
    #nll <- nll - dnorm(log_sd_d_w, mean = log(0.0001), sd = 0.001, log = TRUE)
  }
  
  if(exists("log_sd_d_y")) {
    sd_d_y <- exp(log_sd_d_y)
    #nll <- nll - dnorm(log_sd_d_y, mean = log(0.0001), sd = 0.001, log = TRUE)
  }
  
  if(exists("log_sd_d_yw")) {
    sd_d_yw <- exp(log_sd_d_yw)
    #nll <- nll - dnorm(log_sd_d_yw, mean = log(0.0001), sd = 0.01, log = TRUE)
  }
  
  if(exists("log_sd_d_loc")) {
    sd_loc    <- exp(log_sd_d_loc)
  }
  
  if(exists("log_sd_d_m")) {
    sd_d_m <- exp(log_sd_d_m)
  }
  
  if(exists("log_sd_d_ys")) {
    sd_d_ys <- exp(log_sd_d_ys)
  }
  
  if(exists("log_sd_d_yq")) {
    sd_d_yq <- exp(log_sd_d_yq)
  }
  
  if(exists("log_sd_d_ym")) {
    sd_d_ym <- exp(log_sd_d_ym)
  }
  
  if(exists("log_sd_d_ah") && exists("log_sd_d_bh"))  {
    sd_d_ah <- exp(log_sd_d_ah)
    sd_d_bh <- exp(log_sd_d_bh)
  }
    
  
  if(exists("t_phi_w")){
    phi_w <- 2*plogis(t_phi_w)-1  
  }
  
  if(exists("t_phi_y")){
    phi_y <- 2*plogis(t_phi_y)-1
  }
  
  if(exists("t_phi_yw")){
    phi_yw <- 2*plogis(t_phi_yw)-1
  }
  
  if(exists("t_phi_m")){
    phi_m <- 2*plogis(t_phi_m)-1
  }
  
  if(exists("t_phi_ys")){
    phi_ys <- 2*plogis(t_phi_ys)-1
  }
  
  if(exists("t_phi_yq")){
    phi_yq <- 2*plogis(t_phi_yq)-1
  }
  
  if(exists("t_phi_ym")){
    phi_ym <- 2*plogis(t_phi_ym)-1
  }
  
  if(exists("t_phi_ah") && exists("t_phi_bh"))   {
    phi_ah <- 2*plogis(t_phi_ah)-1
    phi_bh <- 2*plogis(t_phi_bh)-1
  }
  
  if(exists("d_w")) {
    nll <- nll - dnorm(d_w[[1]], 0, sd_d_w, log = TRUE)
    
    dev_d_w  <- c(d_w[[1]] - phi_w * d_w[[length(d_w)]],
                  d_w[-1] - phi_w * d_w[-length(d_w)])
    nll <- nll - sum(dnorm(x = dev_d_w, mean = 0, sd = sd_d_w, log = TRUE))
  }                   
  
  if(exists("d_y")) {
    nll <- nll - dnorm(d_y[1], 0, sd_d_y, log = TRUE)
    dev_d_y  <- d_y[-1] - phi_y * d_y[-length(d_y)]
    nll <- nll - sum(dnorm(x = dev_d_y, mean = 0, sd = sd_d_y, log = TRUE))
  }
  
  if(exists("d_yw")) {
    nll <- nll - dnorm(d_yw[1], 0, sd_d_yw, log = TRUE)
    dev_d_yw <- d_yw[-1] - phi_yw * d_yw[-length(d_yw)]
    nll <- nll - sum(dnorm(x = dev_d_yw, mean = 0, sd = sd_d_yw, log = TRUE))
  }
  
  if(exists("d_m")) {
    nll <- nll - dnorm(d_m[[1]], 0, sd_d_m, log = TRUE)
    
    dev_d_m  <- c(d_m[[1]] - phi_m * d_m[[length(d_m)]],
                  d_m[-1] - phi_m * d_m[-length(d_m)])
    nll <- nll - sum(dnorm(x = dev_d_m, mean = 0, sd = sd_d_m, log = TRUE))
  }
  
  if(exists("d_ys")) {
    nll <- nll - dnorm(d_ys[1], 0, sd_d_ys, log = TRUE)
    dev_d_ys <- d_ys[-1] - phi_ys * d_ys[-length(d_ys)]
    nll <- nll - sum(dnorm(x = dev_d_ys, mean = 0, sd = sd_d_ys, log = TRUE))
  }
  
  if(exists("d_yq")) {
    nll <- nll - dnorm(d_yq[1], 0, sd_d_yq, log = TRUE)
    dev_d_yq <- d_yq[-1] - phi_yq * d_yq[-length(d_yq)]
    nll <- nll - sum(dnorm(x = dev_d_yq, mean = 0, sd = sd_d_yq, log = TRUE))
  }
  
  if(exists("d_ym")) {
    nll <- nll - dnorm(d_ym[1], 0, sd_d_ym, log = TRUE)
    dev_d_ym <- d_ym[-1] - phi_ym * d_ym[-length(d_ym)]
    nll <- nll - sum(dnorm(x = dev_d_ym, mean = 0, sd = sd_d_ym, log = TRUE))
  }

  if(exists("d_loc")) {
    nll <- nll - sum(dnorm(x = d_loc, mean = 0, sd = sd_loc, log = TRUE))
  }
  
  if(exists("d_ah") && exists("d_bh"))   {
    nll <- nll - dnorm(d_ah[1], 0, sd_d_ah, log = TRUE)
    nll <- nll - dnorm(d_bh[1], 0, sd_d_bh, log = TRUE)
    dev_d_ah <- d_ah[-1] - phi_ah * d_ah[-length(d_ah)]
    dev_d_bh <- d_bh[-1] - phi_bh * d_bh[-length(d_bh)]
    nll <- nll - sum(dnorm(x = dev_d_ah, mean = 0, sd = sd_d_ah, log = TRUE))
    nll <- nll - sum(dnorm(x = dev_d_bh, mean = 0, sd = sd_d_bh, log = TRUE))
  }
    
  
  log_b <- vector(mode = "numeric", length = nrow(ind_b))
  
    log_b[] <- log_b0
    
    if(exists("d_loc")) {log_b <- log_b + d_loc[ind_b$ind_loc]}
    if(exists("d_y"))   {log_b <- log_b + d_y[ind_b$ind_year]}
    if(exists("d_w"))   {log_b <- log_b + d_w[ind_b$ind_week]}
    if(exists("d_yw"))  {log_b <- log_b + d_yw[ind_b$ind_yw]}
    if(exists("d_sem")) {log_b <- log_b + d_sem*ind_b$ind_sem2}
    if(exists("d_q"))   {log_b <- log_b + d_q[1]*ind_b$ind_q2}
    if(exists("d_q"))   {log_b <- log_b + d_q[2]*ind_b$ind_q3}
    if(exists("d_q"))   {log_b <- log_b + d_q[3]*ind_b$ind_q4}
    if(exists("d_m"))   {log_b <- log_b + d_m[ind_b$ind_m]}
    if(exists("d_ys"))  {log_b <- log_b + d_ys[ind_b$ind_ys]}
    if(exists("d_yq"))  {log_b <- log_b + d_yq[ind_b$ind_yq]}
    if(exists("d_ym"))  {log_b <- log_b + d_ym[ind_b$ind_ym]}
    if(exists("d_ah") && exists("d_bh"))   {
      t_i <- (ind_b$ind_week - 1) / 52
      log_b <-  log_b + d_ah[ind_b$ind_year] * sin(2 * pi * t_i) +
                        d_bh[ind_b$ind_year] * cos(2 * pi * t_i)
      }
  
  ## Predict individual weight-at-length #######################################
  
  # Transform b
  b <- exp(log_b)
  
  # Individual weight-at-length
  log_W_i_det <- 0
  if(exists("log_g")) log_W_i_det <- dat$ind_gutted*log_g
  log_W_i_det <- log_W_i_det + log_a + b[ind_use$b_Ltru_i]*log(dat$Ltru_i) 
  
  
  
  ## Calculate sample weight likelihood ########################################
  if(!is.null(attr(test, "s_train"))) { # allow for user-defined train set
    s_train <- attr(test, "s_train")
    } else {
      s_train <- setdiff(unique(dat$s), test$s_test)
    }
  
  summary_df_dplyr <- 
    dat %>%
    ungroup() %>%
    # First, add the external vector as a new column
    mutate(exp_log_W_i_det = exp(log_W_i_det)) %>%
    # Now filter and summarize as before
    filter(s %in% s_train) %>%
    group_by(s) %>%
    summarise(
      pred_Wobs_s = sum(exp_log_W_i_det),
      Wobs_s = first(Wobs_s),
      ind_data_source = first(ind_data_source),
      .groups = 'drop' # Recommended to avoid a grouped data frame
    )
  
  # The rest of the calculation remains the same
  logpred_Wobs_s <- log(summary_df_dplyr$pred_Wobs_s)
  log_Wdev_s     <- log(summary_df_dplyr$Wobs_s) - logpred_Wobs_s
  sds_to_use     <- sd_s[summary_df_dplyr$ind_data_source]
  
  nll <- nll - sum(dnorm(x = log_Wdev_s, mean = 0, sd = sds_to_use, log = TRUE))
  
  
  ## Predict weight-at-length from the fitted model ############################
  if (exists("pred_specs")) {

     pred_log_wal <- log_a + b[ind_use$b_pred] * log(pred_specs$length_to_predict)
    
  }
  
  ## Predict test set if it exists #############################################
  if (!is.null(test)) {
    
    dat_test <- dat[dat$s %in% test$s_test,]
    
    log_W_i_det_test <- 0
    if(exists("log_g")) log_W_i_det_test <- log_W_i_det_test + dat_test$ind_gutted*log_g
    log_W_i_det_test <- log_W_i_det_test + log_a + b[ind_use$b_test]*log(dat_test$Ltru_i)
    
    unique_test_s <- unique(dat_test$s)
    pred_Wobs_s_test <- numeric(length = length(unique_test_s))
    
    for(i in 1:length(unique_test_s)) {
      pred_Wobs_s_test[i] <- sum(exp(log_W_i_det_test[dat_test$s == unique_test_s[i]]))
    }
    
    # pred_Wobs_s_test <-
    #   data.frame(log_W_i_det_test = log_W_i_det_test,
    #              s = dat_test$s)# %>%
    #   group_by(s) %>%
    #   summarise(pred = sum(exp(log_W_i_det_test))) %>%
    #   pull(s, pred)
    
  }
    
  
  
  ## Generate final sample fits for exporting ##################################
  # pred_Wobs_s_final <- numeric(length = length(s_train))
  # for(j in s_train){
  #   
  #   ind_s <- which(dat$s == j)
  #   
  #   pred_Wobs_s_final[j] <- sum(exp(log_W_i_det[ind_s]))
  # }
  
  
  ## Report out variables of interest ##########################################
  
  ADREPORT(log_a)
  ADREPORT(log_b)
  ADREPORT(log_sd_s)
  ADREPORT(log_b0)
  if (exists("log_g")) ADREPORT(log_g)
  if (exists("log_sd_d_w")) ADREPORT(log_sd_d_w)
  if (exists("log_sd_d_y")) ADREPORT(log_sd_d_y)
  if (exists("log_sd_d_yw")) ADREPORT(log_sd_d_yw)
  if (exists("log_sd_d_m")) ADREPORT(log_sd_d_m)
  if (exists("log_sd_d_ys")) ADREPORT(log_sd_d_ys)
  if (exists("log_sd_d_yq")) ADREPORT(log_sd_d_yq)
  if (exists("log_sd_d_ym")) ADREPORT(log_sd_d_ym)
  if (exists("log_sd_d_loc")) ADREPORT(log_sd_d_loc)
  if (exists("d_ah") && exists("d_bh")) ADREPORT(sd_d_ah)
  if (exists("d_ah") && exists("d_bh")) ADREPORT(sd_d_bh)  
  if (!is.null(sdrep_pred)) {
    # 1. Start with a vector of TRUE (keep everything)
    keep_mask <- rep(TRUE, nrow(pred_specs))
    
    # 2. Progressively narrow down the mask based on what's in the 'sdrep_pred' list
    if (!is.null(sdrep_pred$loc)) {
      keep_mask <- keep_mask & (pred_specs$loc_to_predict %in% sdrep_pred$loc)
    }
    if (!is.null(sdrep_pred$year)) {
      keep_mask <- keep_mask & (pred_specs$year_to_predict %in% sdrep_pred$year)
    }
    if (!is.null(sdrep_pred$length)) {
      keep_mask <- keep_mask & (pred_specs$length_to_predict %in% sdrep_pred$length)
    }
    if (!is.null(sdrep_pred$yw)) {
      keep_mask <- keep_mask & (pred_specs$yw_to_predict %in% sdrep_pred$yw)
    }
    # 3. Extract the subset using the mask
    # We use which() to get integer indices
    sdrep_pred_idx <- which(keep_mask)
    
    if(length(sdrep_pred_idx) > 0) {
      pred_log_wal_sdrep <- pred_log_wal[sdrep_pred_idx]
      ADREPORT(pred_log_wal_sdrep)
    }
  }
  
  REPORT(ind_b)
  REPORT(log_W_i_det)
  REPORT(pred_log_wal)
  
  if (exists("d_y"))   REPORT(d_y)
  if (exists("d_w"))   REPORT(d_w)
  if (exists("d_yw"))  REPORT(d_yw)
  if (exists("d_loc")) REPORT(d_loc)
  if (exists("d_sem")) REPORT(d_sem)
  if (exists("d_q"))   REPORT(d_q)
  if (exists("d_m"))   REPORT(d_m)
  if (exists("d_ys"))  REPORT(d_ys)
  if (exists("d_yq"))  REPORT(d_yq)
  if (exists("d_ym"))  REPORT(d_ym)
  if (exists("d_ah") && exists("d_bh")) REPORT(d_ah)
  if (exists("d_ah") && exists("d_bh")) REPORT(d_bh)
  if (!is.null(test))  REPORT(pred_Wobs_s_test)
  if (!is.null(test))  REPORT(unique_test_s)
  
  ## Return
  nll
  
}


