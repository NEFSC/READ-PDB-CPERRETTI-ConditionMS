## Pull data from tables #######################################################

pull_data <- function(tables, specs) {

  ## Pull port sampling data ###################################################
  bsm_dat <- 
    tables$CF_BIOSAMPLE_LENGTHS %>%
    mutate(MONTH = as.numeric(MONTH)) %>%
    filter(WGTSAMP != 0,
           SPECIES_ITIS == specs$itis,
           AREA %in% specs$stat_areas,
           !(SPECIES_ITIS == 164712 & LENGTH == 488), # remove cod outlier
           YEAR %in% specs$years
    ) %>%
    mutate(WGTSAMP_KG = WGTSAMP * 0.453592) %>% 
    collect() %>%
    mutate(species_label = specs$species_label,
           stock_label = specs$stock_label)
  
  ## Pull individual port sampling data ########################################
  dmf_dat <-
    tables$BSM_ORGANISM_PARAMETER_VIEW %>%
    left_join(tables$BSM_TALLY_VIEW) %>%
    left_join({tables$BSM_SAMPLES_VIEW %>%
        select(TALLY_NO, TALLY_VESSEL_SEQ,
               SAMPLE_SEQ, SPECIES_ITIS, COMMON_NAME,
               MARKET_DESC, GRADE_DESC)}) %>%
    left_join({tables$BSM_TALLY_VESSELS_VIEW %>% select(TALLY_NO, TALLY_VESSEL_SEQ,
                                                 AREA_CODE)}) %>%
    filter(SAMPLE_SOURCE_CODE == "06",
           SPECIES_ITIS == specs$itis,
           AREA_CODE %in% specs$stat_areas,
           SAMPLING_YR %in% specs$years
           ) %>%
    collect() %>%
    mutate(species_label = specs$species_label,
           stock_label = specs$stock_label)
  
  
  ## Pull survey strata ########################################################
  sv_strata <-
    tables$I_STOCK_SPECIES_J %>%
    filter(SVSPP == specs$svspp) %>%
    dplyr::select(COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, SVSPP) %>%
    distinct() %>%
    mutate(STOCK_ABBREV = specs$stockeff_abbrev) %>%
    left_join({tables$I_SV_STOCK_STRATA_S %>%
        filter(STOCK_ABBREV == specs$stockeff_abbrev,
               PURPOSE_CODE %in% specs$survey_purpose_code) %>%
        distinct(SPECIES_ITIS, STOCK_ABBREV, PURPOSE_CODE,
                 SEASON, STRATUM, SEX_TYPE)}) %>%
    left_join(tables$I_SV_SURVEY_SEASON_C) %>%
    collect() %>%
    mutate(keep_stratum = TRUE) 
  
  
  sv_strata_vector <-
    sv_strata %>%
    select(STRATUM) %>%
    as.matrix()
  
  ## Pull survey data #####################################
  srv_dat <- 
    tables$UNION_FSCS_SVBIO %>%
    left_join(tables$SVDBS_CRUISES) %>%
    filter(PURPOSE_CODE %in% specs$survey_purpose_code,
        STATUS_CODE == 10, # 10: cruise is available in SVDBS
        YEAR %in% specs$years,
        SVSPP == specs$svspp,
        #!is.null(AGE),
        !is.null(LENGTH),
        !is.null(INDWT),
        INDWT > 0,
        LENGTH > 0,
        STRATUM %in% sv_strata_vector #First filter of strata (not season/survey specific)
    ) %>%
    left_join(tables$UNION_FSCS_SVSTA) %>%
    collect() %>%
    left_join(sv_strata) %>%
    filter(keep_stratum) %>% # Final filter of strata (season/survey specific)
    rename(MONTH = EST_MONTH) %>%
    mutate(MONTH = as.numeric(MONTH)) %>%
    filter(!is.na(MONTH),
           !is.na(AREA))  %>%
    mutate(species_label = specs$species_label,
           stock_label = specs$stock_label)
  
  
  
  return(list(bsm_dat = bsm_dat, srv_dat = srv_dat, dmf_dat = dmf_dat))
}