# Set specs ####################################################################
set_specs <- function(itis, stock, years) {
  
  established_specs <- data.frame(itis = c(164744, 164744, 164712, 164712, 164727),
                                  stock = c("GOM", "GBK",  "WGOM", "GBK",  "UNIT"))
  
  if(!any(established_specs$itis == itis & established_specs$stock == stock)){
    stop(paste0("This itis-stock combination (", itis, "-", stock,")",
         " does not have specifcations set yet. Add them to set_specs.R."))
  }
  
  specs <- list()
  specs$itis <- itis
  specs$years <- years
  if(specs$itis == 164744 & stock == "GOM") {
    specs$species_label <- "haddock"
    specs$stock_label <- "GOM"
    specs$svspp <- "074" # 074 is haddock
    specs$stockeff_abbrev <- "GOMAL"
    specs$stat_areas <- c(464, 465, 467, 510, 511, 512, 513, 514, 515)
    specs$stockeff_mode <- "STOCKEFF_PRE_PROD"
    specs$survey_purpose_code <- c(10)
    
  }
  if(specs$itis == 164744 & stock == "GBK") {
    specs$species_label <- "haddock"
    specs$stock_label <- "GBK"
    specs$svspp <- "074" # 074 is haddock
    specs$stockeff_abbrev <- "GBK"
    specs$stat_areas <- c(520, 521, 522, 523, 524, 525, 526, 530, 533, 534, 537,
                          538, 539, 540, 541, 542, 543, 550, 551, 552, 560, 561,
                          562, 600, 610, 611, 612, 613, 614, 615, 616, 620, 621,
                          622, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632,
                          633, 634, 635, 636, 637, 638, 639, 640, 650, 660, 670,
                          680, 700)
    specs$stockeff_mode <- "STOCKEFF"
    specs$survey_purpose_code <- c(10)
  }
  if(specs$itis == 164712 & stock == "WGOM") {
    specs$species_label <- "cod"
    specs$stock_label <- "WGOM"
    specs$svspp <- "073" # 073 is cod
    specs$stockeff_abbrev <- "WGOM"
    specs$stat_areas <- c(510, 513, 514, 515, 520, 521, 526, 541)
    specs$stockeff_mode <- "STOCKEFF"
    specs$survey_purpose_code <- c(10)
    
    specs$null <- list()
    specs$null$model_effects <- c("gut", "semester")
    specs$null$inits <- list(log_g = log(1/1.17), 
                             log_a = log(0.000006),#c(log(0.00000588), log(6.40E-06)),
                             log_b0 = log(3.125071186),
                             d_sem  = log(3.12621468) - log(3.125071186))
    specs$null$map  <- list(log_g  = factor(NA),
                            log_a  = factor(NA),#factor(c(NA, NA)),
                            log_b0 = factor(NA),
                            d_sem  = factor(NA))
  }
  if(specs$itis == 164712 & stock == "GBK") {
    specs$species_label <- "cod"
    specs$stock_label <- "GBK"
    specs$svspp <- "073" # 073 is cod
    specs$stockeff_abbrev <- "GBK"
    specs$stat_areas <- c(464, 522, 523, 524, 525, 542, 543, 551, 552, 561, 562)
    specs$stockeff_mode <- "STOCKEFF"
    specs$survey_purpose_code <- c(10)
  }
  if(specs$itis == 164727 & stock == "UNIT") {
    specs$species_label <- "pollock"
    specs$stock_label <- ""
    specs$svspp <- "075" # 075 is pollock
    specs$stockeff_abbrev <- "UNIT"
    specs$stat_areas <- c(500, 510, 511, 512, 513, 514, 515, 520, 521,
                          522, 523, 524, 525, 526, 530, 533, 534, 537,
                          538, 539, 540, 541, 542, 543, 550, 551, 552,
                          560, 561, 562, 600, 610, 611, 612, 613, 614,
                          615, 616, 620, 621, 622, 623, 624, 625, 626,
                          627, 628, 629, 630, 631, 632, 633, 634, 635,
                          636, 637, 638, 639, 640, 650, 660, 670, 680)
    specs$stockeff_mode <- "STOCKEFF"
    specs$survey_purpose_code <- c(10)
  }
  
  
  
  return(specs)
}



