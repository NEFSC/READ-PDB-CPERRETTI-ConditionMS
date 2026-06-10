## Connect to database and grab tables #########################################
grab_tables <- function(specs, username = NULL, password = NULL) {
  
  #Close old connection if it exists
  if (exists("conn")) try(dbDisconnect(conn), silent = TRUE)
  
  
  conn <- dbConnect(drv = dbDriver("Oracle"),
                    username = ifelse(is.null(username), Sys.getenv("DB_USER"), username), 
                    password = ifelse(is.null(password), Sys.getenv("DB_PASS"), password),  
                    dbname = "NEFSC_pw_oraprod")
  
  tables <- list()
  
  # Port biosamples
  tables$CF_BIOSAMPLE_LENGTHS <- tbl(conn, in_schema("BSM", "CF_BIOSAMPLE_LENGTHS_VIEW"))
  
  # Survey strata for stock
  tables$I_STOCK_SPECIES_J <- tbl(conn, in_schema("STOCKEFF", "I_STOCK_SPECIES_J"))
  tables$I_SV_STOCK_STRATA_S <- tbl(conn, in_schema(specs$stockeff_mode, "I_SV_STOCK_STRATA_S"))
  tables$I_SV_SURVEY_SEASON_C <- tbl(conn, in_schema(specs$stockeff_mode, "I_SV_SURVEY_SEASON_C"))
  
  # Survey individual biosamples
  tables$UNION_FSCS_SVBIO <- tbl(conn, in_schema("SVDBS", "UNION_FSCS_SVBIO"))
  tables$SVDBS_CRUISES <- tbl(conn, in_schema("SVDBS", "SVDBS_CRUISES"))
  tables$UNION_FSCS_SVSTA <- tbl(conn, in_schema("SVDBS", "UNION_FSCS_SVSTA"))
  
  # Port individual biosamples
  tables$BSM_ORGANISM_PARAMETER_VIEW <- tbl(conn, in_schema("BSM", "BSM_ORGANISM_PARAMETER_VIEW"))
  tables$BSM_TALLY_VIEW <- tbl(conn, in_schema("BSM", "BSM_TALLY_VIEW"))
  tables$BSM_SAMPLES_VIEW <- tbl(conn, in_schema("BSM", "BSM_SAMPLES_VIEW"))
  tables$BSM_TALLY_VESSELS_VIEW <- tbl(conn, in_schema("BSM", "BSM_TALLY_VESSELS_VIEW"))
  
  return(tables)
  
}
