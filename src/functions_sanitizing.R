################################################################################
#
#functions for qPCR analysis - COVID19 detection
#Sanitizing inputs
#by: INMEGEN Computational Genomics Dept
#Guillermo de Anda Jauregui gdeanda@inmegen.edu.mx
#
################################################################################

#for ease of convention, these functions evaluate 
#TRUE  if they pass the test
#FALSE  if they fail the test

###CheckResultsEDS 
CheckResultsEDS <- function(eds){
  #takes a path to an eds 
  #if it can read analysis_result, returns TRUE, else FALSE
  eds.file <- try(unz(description = eds, filename = "apldbio/sds/analysis_result.txt"), silent = T)
  #if(class(eds.file)=="try-error"){
  #  message("the error is at the unz level")
  #  return(FALSE)
  #}
  
  analysis_text <- try(suppressWarnings(read_lines(eds.file)), silent = T)
  #print(analysis_text)
  if(class(analysis_text)=="try-error"){
    message("EDS does not contain analysis_result.txt file")
    return(FALSE)
  }
  
  return(TRUE)
}

###CheckNamesEDS

CheckNamesEDS <- function(eds){
  #takes a path to an eds 
  #extracts and checks that sample names are legible 
  eds.file <- unz(description = eds, filename = "apldbio/sds/analysis_result.txt")
  analysis_text <- read_lines(eds.file)
  #find the lines with the Delta Rn
  delta_rn.id <- which(str_detect(string = analysis_text, pattern = "Delta Rn"))
  
  ### name them with the info that is two rows before
  
  nomen <- 
    analysis_text[delta_rn.id - 2] %>% 
    sapply(FUN = function(i){
      intermedio <- str_split(string = i, pattern = "\t")
      intermedio <- unlist(intermedio, recursive = F)
      intermedio[2]
    })
  
  
  #test that the sample id has no special characters
  test_for_special = grepl(pattern = "[^a-zA-Z0-9-]", x = nomen)
  my_r = all(test_for_special == FALSE) #if there is one sample with bad names, it evaluates FALSE
  #pass is TRUE
  #fail is FALSE
  return(my_r)
  
}

### Check correct probes 

CheckProbesEDS <- function(eds, my_probes){
  #takes a path to an eds 
  #extracts and checks that sample names are legible 
  eds.file <- unz(description = eds, filename = "apldbio/sds/analysis_result.txt")
  analysis_text <- read_lines(eds.file)
  #find the lines with the Delta Rn
  delta_rn.id <- which(str_detect(string = analysis_text, pattern = "Delta Rn"))
  
  ### name them with the info that is two rows before
  
  nomen <- 
    analysis_text[delta_rn.id - 2] %>% 
    lapply(FUN = function(i){
      intermedio <- str_split(string = i, pattern = "\t")
      intermedio <- unlist(intermedio, recursive = F)
      df = data.frame(sample = intermedio[2], probe = intermedio[3])
    }) 
  
  nomen = suppressWarnings(bind_rows(nomen))
  
  los_probes = 
  nomen %>% 
    group_by(sample) %>% 
    group_map(~ unique(.x$probe))
  
  #check they are the same probes with jaccard
  j_probes <-
  lapply(los_probes, function(i){
    
    J = length(intersect(i, my_probes))/length(union(i, my_probes))
    
    return(J==1)
  })
  
  #if all samples have all probes, TRUE
  resultado = suppressWarnings(all(j_probes))
  return(resultado)
  
}

### Check correct controls and number of controls

CheckControlsEDS <- function(eds, controls = c("NTC", "PTC", "CRE")){
  
  
  eds.file <- unz(description = eds, filename = "apldbio/sds/analysis_result.txt")
  analysis_text <- read_lines(eds.file)
  #find the lines with the Delta Rn
  delta_rn.id <- which(str_detect(string = analysis_text, pattern = "Delta Rn"))
  
  #extract those, put them as a vector  
  ### splits them in a list
  list_deltaRN <- analysis_text[delta_rn.id] %>% 
    str_replace(pattern = "Delta Rn values\t", replacement = "") %>% 
    sapply(FUN = function(i){
      str_split(string = i, pattern = "\t") 
    }) 
  
  ### makes them numeric
  list_deltaRN <- 
    lapply(list_deltaRN, FUN = function(i){
      i <- as.numeric(i)
      names(i) <- 1:length(i)
      return(i)
    })
  
  ### name them with the info that is two rows before
  
  nomen <- 
    analysis_text[delta_rn.id - 2] %>% 
    sapply(FUN = function(i){
      intermedio <- str_split(string = i, pattern = "\t")
      intermedio <- unlist(intermedio, recursive = F)
      paste(intermedio[1], intermedio[2], intermedio[3], sep = "_")
    })
  
  names(list_deltaRN) <- nomen
  
  ### make a data frame 
  df_deltaRN <- 
    list_deltaRN %>% 
    as_tibble()
  
  ### add the cycles
  
  df_deltaRN <-
    df_deltaRN %>% 
    mutate(cycles = 1:nrow(df_deltaRN))
  
  tdrn <- 
  df_deltaRN %>% 
    pivot_longer(cols = -cycles,
                 names_to = "sample.id",
                 values_to = "value") %>%
    separate(col = sample.id, sep = "_", into = c("well", 
                                                  "sample.label", 
                                                  "probe"),
             remove = F)
  
  ### check that there is the correct number of controls 
  check_control_labels <- 
  tdrn %>% 
    group_by(sample.label) %>% 
    tally() %>% 
    filter(sample.label%in%controls)
  
  ### SAME NAMES AS EXPECTED
  if (nrow(check_control_labels) != length(controls)){
    return("SOME QC CONTROLS ARE MISSING; OR THE NAME OF THE CONTROLS DO NOT CORRESPOND")
  }
  
  ### IF SOME CONTROL IS IN MORE THA ONE WELL
  cycles_per_control <- check_control_labels %>% 
    select(n) %>%  
    unique()
  
  if (nrow(cycles_per_control) != 1){
    return("SOME QC CONTROLS DON'T HAVE THE SAME NUMBER OF CYCLES OR WELLS")
  }
  
  return("PASS")

}