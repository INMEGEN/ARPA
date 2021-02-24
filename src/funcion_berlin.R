library(tidyverse)
library(vroom)
library(janitor)
source("src/functions.R")
source("src/plots.R")

source("src/functions_adjustment.R")
source("src/getVolumes.R")

funcion_berlin <- function(input_eds, 
          output){
  
  
  ########
  #define probes
  ########
  
  berlin_probes <- try(c("Gen E", "Gen RNasaP"))
  names(berlin_probes) <- try(berlin_probes)
  
   
  ##############################################################################
  #define QC names 
  #this is hard code for now, until we get feedback on actual names and roles
  #used by analytics team
  ##############################################################################
  
  qc_names = c("NTC", "PTC", "CRE")
  
  
  my_deltaRN <- try(tidy_deltaRN(input_eds)) #read deltaRN from EDS file 
  
  # analyze curves, determine sample ct
  test.samples <- try(test.plate(tdrn = my_deltaRN, probes = berlin_probes))
  names(test.samples) <- try(c("sample_name", "gen_e", "gen_r_nasa_p"))
  
  #curve plot
  plate_curves <- 
    try(pivot_deltaRN(my_deltaRN) %>% 
    plot_deltaRN.long())
  
  #qc results
  qc_results <-
    tryCatch( expr = {plate_qc.berlin(tdrn = my_deltaRN, 
                                all_probes = berlin_probes)},
              error = function(e){return(as.character(e))}
              
  )
  

 #do results for samples 
  
  test.results <- 
    tryCatch(expr = {test.samples %>% 
        mutate(classification = case_when(gen_e <= 40 & gen_r_nasa_p <= 40 ~ "Positivo",
                                          gen_e > 40 & gen_r_nasa_p <= 40 ~ "Negativo",
                                          gen_r_nasa_p > 40 ~ "Repetir"
        ))},
             error = function(e){return(as.character(e))}
             )
     

  #############
  #list of single figure plots
  #############
  
  all_samples <- try(c(test.results$sample_name %>% unique, qc_names))
  names(all_samples) <- try(c(all_samples))
  
  single_plots <- 
    tryCatch(expr = {lapply(all_samples, FUN = function(i){
      sample_curve.berlin.manual(tdrn = my_deltaRN, 
                                 sample_id = i, 
                                 probes = berlin_probes
      )
    })},
             error = function(e){return(as.character(e))}
             )
    
  
  ################################################################################
  # Modify 99 and Inf to >45 sample results
  ################################################################################
  if (!is.character(test.results)){
    test.results = test.results %>% 
      mutate(gen_e = ifelse(gen_e == "99", ">40", gen_e)) %>% 
      mutate(gen_e = ifelse(gen_e == "Inf", ">40", gen_e)) %>% 
      mutate(gen_r_nasa_p = ifelse(gen_r_nasa_p == "99", ">40", gen_r_nasa_p)) %>% 
      mutate(gen_r_nasa_p = ifelse(gen_r_nasa_p == "Inf", ">40", gen_r_nasa_p))
  }
  
  ################################################################################
  # Modify 99 and Inf to >45 QC results, remove warnings colums
  ################################################################################
  if (!is.character(qc_results)){
    qc.values = qc_results$qc.values
    qc.values = qc.values %>% 
      mutate(gen_e = ifelse(gen_e == "99", ">40", gen_e)) %>% 
      mutate(gen_e = ifelse(gen_e == "Inf", ">40", gen_e)) %>% 
      mutate(gen_r_nasa_p = ifelse(gen_r_nasa_p == "99", ">40", gen_r_nasa_p)) %>% 
      mutate(gen_r_nasa_p = ifelse(gen_r_nasa_p == "Inf", ">40", gen_r_nasa_p)) %>% 
      select(-warnings)
    qc_results$qc.values <- qc.values
  }
  
  ################################################################################
  #Create list output
  ################################################################################
  
  results_list <- list(
    test_results = test.results,
    qc_results = qc_results, 
    single_plots = single_plots
  )
  
  return(results_list)
  
}
