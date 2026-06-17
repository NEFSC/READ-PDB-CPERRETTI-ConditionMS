library(ROracle)
library(dplyr)
library(dbplyr)
library(tidyr)
library(RTMB)
library(ggplot2)
library(purrr)
library(furrr)
library(ggh4x)


# Set wd to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom functions ########################################################
fs::dir_ls("functions", glob = "*.R") %>% 
  walk(source)
cmb <- function(f, d) function(p) f(p, d)

# Load data and models (to allow for skipping model-fitting if desired) ########

load("dat_and_mods_WGOM_cod_2026-05-08.RData") # run this for cod
# load("dat_and_mods_GOM_haddock_2026-05-08.RData") # run this for haddock

# Make plots of cleaned data ###################################################
plot_cleaned_data(dat, save_plots = TRUE)


# Fit all models ###############################################################
spec_list <- list(
  g       = list(model_effects = "gut"),
  gs      = list(model_effects = c("gut", "semester")),
  gm      = list(model_effects = c("gut", "month")),
  gmy     = list(model_effects = c("gut", "month", "year")),
  gwy     = list(model_effects = c("gut", "week", "year")),
  `gy-w`  = list(model_effects = c("gut", "year-week")),
  `gy-wl` = list(model_effects = c("gut", "year-week", "loc")),
  `ghy-w` = list(model_effects = c("gut", "harmonic", "year-week")),
  `ghy-wl` = list(model_effects = c("gut", "harmonic", "year-week", "loc"))
)




plan(multisession, workers = availableCores() - 1)
message("Fitting all models")
tictoc::tic()
mods <- future_imap(spec_list, 
                    ~fit_lw(dat = dat,
                            model_effects = .x$model_effects,
                            model_name = .y,
                            do_sdrep = TRUE,
                            inits = .x$inits,
                            map = .x$map,
                            silent = TRUE,
                            sdrep_pred = list(loc = "514",
                                              length = round(median(dat$Ltru_i)))),
                    .options = furrr_options(seed = TRUE))
tictoc::toc()
plan(sequential)


# Calculate convergence and information criteria scores ########################
(ic <-
   purrr::map_dfr(mods, calc_ic, dat = dat, .id = "mod") %>%
   mutate(dAIC = round(AIC - min(AIC, na.rm = TRUE)),
          dBIC = round(BIC - min(BIC, na.rm = TRUE))) %>%
   select(mod, converged, dAIC, dBIC))

param_counts <- as.data.frame(t(sapply(mods, function(m) {
  obj_fun <- m$obj 
  
  c(
    fixed_effects  = length(obj_fun$par),
    random_effects = length(obj_fun$env$random),
    total_params   = length(obj_fun$env$last.par)
  )
})))

# Add the model names as a column
param_counts$model_name <- rownames(param_counts)

print(param_counts)


# Perform cross-validation for all models ######################################
mods_cv_k <- do_crossval(mods,
                         dat, k = 10, kfold = TRUE, do_sdrep = FALSE, 
                         reduce_output = TRUE)

mods_cv_ts <- do_crossval(mods,
                          dat, ts = TRUE, k = 10, max_years_ahead = 5,
                          do_sdrep = FALSE, reduce_output = TRUE)

mods_cv_lodo <- do_crossval(mods,
                            dat, lodo = TRUE, do_sdrep = FALSE, 
                            reduce_output = FALSE)



# Plot b parameter estimates ###################################################
plot_b(mods = mods[9],
       dat = dat, 
       plot_CI = TRUE,
       loc = 514)



# Plot weight-at-length ########################################################
plot_wal(mods =mods[9],
         dat = dat,
         plot_CI = TRUE,
         loc = 514)

plot_wal_decadal(mods = mods, dat = dat)

# Calculate cross-validation error #############################################
crossval_out_k <-
  calc_crossval_err(mods_cv_k,
                    dat,
                    groups_to_use = c("name_and_effects"
                    )
  )


crossval_out_ts <- 
  calc_crossval_err(mods_cv_ts,
                    dat,
                    groups_to_use = c("name_and_effects", 
                                      "data_source"
                    )
  )


crossval_out_lodo <- 
  calc_crossval_err(mods_cv_lodo,
                    dat,
                    groups_to_use = c("name_and_effects", 
                                      "data_source"
                    )
  )


# Plot cross-validation error summary statistics ###############################
plot_crossval_err(crossval_out_k, dat = dat) 

plot_crossval_err(crossval_out_lodo, dat = dat)

plot_combined_crossval_err(list(crossval_out_k, crossval_out_ts, crossval_out_lodo))

# Plot cross-validation predictions vs observed ################################
plot_crossval_obspred(crossval_results = crossval_out_k$crossval_results %>% 
                        filter(model_name %in% c("ghy-wl")))

plot_crossval_obspred(crossval_out_ts$crossval_results %>% 
                        filter(model_name %in% c("ghy-wl", "gs"))
)


# Plot fixed effect estimates ##################################################
plot_fe(mod = mods[9], dat = dat, make_table = TRUE)

# Plot random effect estimates #################################################
plot_re(mod = mods[9], dat = dat)

# Tabulate model bias comparison (in sample) ###################################
summarise_model_fit(mods, dat)

# Plot fit vs observed for each data source for a single model #################
plot_obsfit(mods[[9]], dat)
            

