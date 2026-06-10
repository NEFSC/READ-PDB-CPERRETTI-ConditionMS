calc_ic <- function(mod, dat) {
  
  ic <- data.frame(effects = paste(mod$model_effects, collapse = " "), 
                   converged = FALSE,
                   AIC = NA, 
                   #AICc = NA, 
                   BIC = NA#, 
                   #DIC = NA
                   )
  
  if(mod$opt$convergence == 0 & mod$pdHess) {
    ic$converged <- TRUE
    
    k <- length(mod$opt$par)
    ll <- -mod$opt$objective
    n <- nrow(dat)
    dev <- -2*ll
    
    
    ic$AIC <- 2*k - 2*ll
    #ic$AICc <-  2*k - 2*ll + (2 * k * (k + 1)) / (n - k - 1)
    ic$BIC <- k * log(n) -2*ll
    #ic$DIC <- mod$pl$dev + 0.5*mod$plsd$dev^2  
  }
  
  return(ic)
}