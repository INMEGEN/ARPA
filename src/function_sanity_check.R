
source("src/functions_sanitizing.R")

funcion_sanity_checks <- function(input_eds){
  
  #####################
  #Sanity checks 
  #####################
  
  ### check eds has results
  
  has_results <- try(CheckResultsEDS(eds = input_eds))
  if(has_results == FALSE){
    return("THE EDS HAS NO RESULTS")
  }
  
  ### check sample names in eds have no special characters (only AZaz09 and -)
  
  #has_CleanNames <- CheckNamesEDS(eds = input)
  #if(has_CleanNames == FALSE){
  #  return("special_characters_in_names")
  #}
  
  ########
  #define probes
  ########
  
  berlin_probes <- try(c("Gen E", "Gen RNasaP"))
  names(berlin_probes) <- try(berlin_probes)
  
  ### check that all samples have all probes
  
  has_allProbes <- try(CheckProbesEDS(eds = input_eds, my_probes = berlin_probes))
  if(has_allProbes == FALSE){
    return("SOME SAMPLES HAVE MISSING PROBES; OR THE PROBE NAMES DO NOT CORRESPOND")
  }
  
  ##############################################################################
  #define QC names 
  #this is hard code for now, until we get feedback on actual names and roles
  #used by analytics team
  ##############################################################################
  
  qc_names = c("NTC", "PTC", "CRE")
  
  has_all_qc <- try(CheckControlsEDS(eds = input_eds, controls = qc_names))
  if(has_all_qc != "PASS"){
    return(has_all_qc)
  }
  
  ##### IF ALL CONFIRAMTIONS ARE TRUE, RETURN TRUE
  return("PASS")
}
