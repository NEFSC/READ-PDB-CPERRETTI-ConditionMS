calc_crossval_err <- function(mods_crossval,
                              dat, groups_to_use) {
  
  
  crossval_results <- 
    map_dfr(mods_crossval, pull_crossval_out, dat) %>%
    left_join({dat %>% distinct(s, .keep_all = TRUE)}) %>%
    { if (unique(.$is_tscv)) group_by(., fold) %>%
        filter(YW <= (min(YW) + max_years_ahead*365)) else . }
  
  crossval_err <- 
    crossval_results %>%
    distinct(species_label, stock_label, test_type, model_name, name_and_effects,
             fold, nlminb_converged,
             data_source, s, YW, Wobs_s, pred_Wobs_s_test) %>%
    mutate(err = pred_Wobs_s_test - Wobs_s,
           err_pc = err/Wobs_s * 100) %>%
    group_by_at(c("test_type", groups_to_use)) %>%
    reframe(msa  = calc_msa(pred_Wobs_s_test, Wobs_s),
            msa_90ci = quantile(boot::boot(dat = data.frame(predicted = pred_Wobs_s_test, 
                                                            observed  = Wobs_s), 
                                           statistic=calc_msa_boot, R=1000)$t, 
                                probs = c(0.05, 0.95)),
            sspb = calc_sspb(pred_Wobs_s_test, Wobs_s),
            sspb_90ci = quantile(boot::boot(dat = data.frame(predicted = pred_Wobs_s_test, 
                                                             observed  = Wobs_s), 
                                            statistic=calc_sspb_boot, R=1000)$t, 
                                 probs = c(0.05, 0.95)),
            mse = mean(err^2),
            rmse = mse^0.5,
            mae = mean(abs(err)),
            mape = mean(abs(err_pc)),
            mpe  = mean(err_pc),
            mape_se = sd(abs(err_pc))/sqrt(n()),
            mpe_se  = sd(err_pc)/sqrt(n()),
            mape_90hi = mape + 1.96*mape_se,
            mape_90lo = mape - 1.96*mape_se,
            mpe_90hi = mpe + 1.96*mpe_se,
            mpe_90lo = mpe - 1.96*mpe_se) %>%
    group_by_at(c("test_type", groups_to_use)) %>%
    mutate(sspb_90lo = min(sspb_90ci),
           sspb_90hi = max(sspb_90ci),
           msa_90lo = min(msa_90ci),
           msa_90hi = max(msa_90ci)) %>%
    distinct(across(-c(msa_90ci, sspb_90ci)))
  
  crossval_out <- list(crossval_results = crossval_results, 
                       crossval_err = crossval_err)
  
  # Summary table with original list ordering
  # Get the original order from the names of the list
  original_model_order <- map_chr(mods_crossval, function(x) {
    paste0(x$model_name, ": ", toString(x$model_effects))
  }) %>% unique()
  
  summary_table <- 
    crossval_results %>%
    distinct(name_and_effects, fold, nlminb_converged) %>%
    group_by(name_and_effects) %>%
    summarise(`convergence rate (%)` = 100 * sum(nlminb_converged) / length(fold)) %>%
    left_join(crossval_err %>% select(name_and_effects, msa, sspb), by = "name_and_effects") %>%
    # Use factor levels to force the original order
    mutate(name_and_effects = factor(name_and_effects, levels = original_model_order),
           msa = round(msa, 2),
           sspb = round(sspb, 2)) %>%
    arrange(name_and_effects) %>%
    rename(MSA = msa,
           SSPB = sspb)
  
  pander::pander(summary_table,
                 caption = paste("Model performance and convergence across", unique(crossval_results$test_type), "fits"),
                 split.table = Inf
  )
  
  return(crossval_out)
}