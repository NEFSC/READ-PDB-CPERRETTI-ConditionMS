pull_fe <- function(mod) {
  
  ests <-
    tibble(variable = "a",
           value = exp(mod$pl$log_a),
           CI_90lo  = exp(mod$pl$log_a - 1.645*mod$plsd$log_a),
           CI_90hi  = exp(mod$pl$log_a + 1.645*mod$plsd$log_a)) %>%
    bind_rows(tibble(variable = paste(unique(dat$data_source)[unique(dat$ind_data_source)],
                                      "obs. error sd"),
                     value = exp(mod$pl$log_sd_s),
                     CI_90lo  = exp(mod$pl$log_sd_s - 1.645*mod$plsd$log_sd_s),
                     CI_90hi  = exp(mod$pl$log_sd_s + 1.645*mod$plsd$log_sd_s))) %>%
    {
      if ("log_g" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "whole-to-gutted multiplier",
                            value = exp(mod$pl$log_g),
                            CI_90lo  = exp(mod$pl$log_g - 1.645*mod$plsd$log_g),
                            CI_90hi  = exp(mod$pl$log_g + 1.645*mod$plsd$log_g)))  
      } else .
    } %>%
    {
      if ("log_sd_d_w" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "week effect sd",
                            value = exp(mod$pl$log_sd_d_w),
                            CI_90lo  = exp(mod$pl$log_sd_d_w - 1.645*mod$plsd$log_sd_d_w),
                            CI_90hi  = exp(mod$pl$log_sd_d_w + 1.645*mod$plsd$log_sd_d_w)))
      } else .
    } %>%
    {
      if ("log_sd_d_y" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "year effect sd",
                            value = exp(mod$pl$log_sd_d_y),
                            CI_90lo  = exp(mod$pl$log_sd_d_y - 1.645*mod$plsd$log_sd_d_y),
                            CI_90hi  = exp(mod$pl$log_sd_d_y + 1.645*mod$plsd$log_sd_d_y)))
      } else .
    } %>%
    {
      if ("log_sd_d_yw" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "year-week effect sd",
                            value = exp(mod$pl$log_sd_d_yw),
                            CI_90lo  = exp(mod$pl$log_sd_d_yw - 1.645*mod$plsd$log_sd_d_yw),
                            CI_90hi  = exp(mod$pl$log_sd_d_yw + 1.645*mod$plsd$log_sd_d_yw)))
      } else .
    } %>%
    {
      if ("log_sd_d_ys" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "year-semester effect sd",
                            value = exp(mod$pl$log_sd_d_ys),
                            CI_90lo  = exp(mod$pl$log_sd_d_ys - 1.645*mod$plsd$log_sd_d_ys),
                            CI_90hi  = exp(mod$pl$log_sd_d_ys + 1.645*mod$plsd$log_sd_d_ys)))
      } else .
    } %>%
    {
      if ("log_sd_d_yq" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "year-quarter effect sd",
                            value = exp(mod$pl$log_sd_d_yq),
                            CI_90lo  = exp(mod$pl$log_sd_d_yq - 1.645*mod$plsd$log_sd_d_yq),
                            CI_90hi  = exp(mod$pl$log_sd_d_yq + 1.645*mod$plsd$log_sd_d_yq)))
      } else .
    } %>%
    {
      if ("log_sd_d_ym" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "year-month effect sd",
                            value = exp(mod$pl$log_sd_d_ym),
                            CI_90lo  = exp(mod$pl$log_sd_d_ym - 1.645*mod$plsd$log_sd_d_ym),
                            CI_90hi  = exp(mod$pl$log_sd_d_ym + 1.645*mod$plsd$log_sd_d_ym)))
      } else .
    } %>%
    {
      if ("log_sd_d_m" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "month effect sd",
                            value = exp(mod$pl$log_sd_d_m),
                            CI_90lo  = exp(mod$pl$log_sd_d_m - 1.645*mod$plsd$log_sd_d_m),
                            CI_90hi  = exp(mod$pl$log_sd_d_m + 1.645*mod$plsd$log_sd_d_m)))
      } else .
    } %>%
    {
      if ("log_sd_d_loc" %in% names(mod$pl)) {
        bind_rows(., tibble(variable = "location effect sd",
                            value = exp(mod$pl$log_sd_d_loc),
                            CI_90lo  = exp(mod$pl$log_sd_d_loc - 1.645*mod$plsd$log_sd_d_loc),
                            CI_90hi  = exp(mod$pl$log_sd_d_loc + 1.645*mod$plsd$log_sd_d_loc)))
      } else .
    } %>%
    mutate(param_type = ifelse(variable %in% c("a", "whole-to-gutted multiplier"),
                               "other parameters", ifelse(grepl("effect", variable), 
                                                          "random effect controllers",
                                                          "observation error controllers"))) %>%
    mutate(fold_name = ifelse("fold" %in% names(mod$test), paste(" fold", mod$test$fold), ""),
           fold_name = ifelse("fold" %in% names(mod$test) && grepl("eave-one-dataset", unique(mod$test$test_type)),
                              paste0(", leave out ", unique(dat$data_source[dat$s %in% mod$test$s_test])),
                              fold_name),
           fold_name = ifelse("fold" %in% names(mod$test) && grepl("series", unique(mod$test$test_type)),
                              paste0(", leave out ", substr(max(dat$YW)-365*unique(mod$test$fold) + 1, 1, 10), "+"),
                              fold_name),
           Model = paste0(mod$model_name, ": ", paste(mod$model_effects, collapse = " "), fold_name))
  
  return(ests)
}