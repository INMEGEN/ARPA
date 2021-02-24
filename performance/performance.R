library(shiny)
library(shinythemes)
library(shinyFiles)
library(DT)
library(data.table)


setwd("/Users/laura/Desktop/COVID-19/aplicacion-ARPA/ARPA/")

source("src/functions.R")
source("src/funcion_berlin.R")
source("src/function_sanity_check.R")
source("src/function_reports.R")



eds_files <- list.files("performance/eds/", pattern = ".eds")
eds_files <- paste("performance/eds", eds_files, sep = "/")


results.list <- lapply(eds_files, function(x){
  date <- strsplit(strsplit(x, "/")[[1]][3], " ")[[1]][1]
  all_results <- funcion_berlin(x, output = "performance/out/" )
  test_results <- all_results$test_results
  test_results$date <- date
  return(test_results)
})


results.arpa <- rbindlist(results.list)

manual <- read.table("performance/IBT_batch2_simple.tsv", colClasses = c("character"))

manual.class <- manual %>% 
  mutate(id = paste(V1, V2, sep = "-")) %>% 
  mutate(V3 = ifelse(V3 == "Posiivo", "Positivo", V3)) %>% 
  rename(manual = V3) %>% 
  select(id, manual)


comparison <- results.arpa %>% 
  mutate(id = paste(date, sample_name, sep="-")) %>% 
  inner_join(manual.class, by = c("id" = "id"))


dim(comparison)
table(comparison$classification, comparison$manual)


##### TYPE 1 ERROR

comparison %>% 
  filter(classification == "Positivo") %>% 
  filter(manual == "Negativo")


##### TYPE 2 ERROR

comparison %>% 
  filter(classification == "Repetir") %>% 
  filter(manual == "Negativo")

tp = 36
tn = 100
fp = 2
fn = 0
tpr = tp/(tp+fn)
tnr = tn/(tn+fp)
ppv = tp/(tp+fp)
npv = tn/(tn+fn)

write.table(comparison, file = "performance/out/Results_ARPA_performance.txt", 
            col.names = TRUE, row.names = FALSE, quote = FALSE)



##### PLOTS
plots.list <- lapply(eds_files, function(x){
  date <- strsplit(strsplit(x, "/")[[1]][3], " ")[[1]][1]
  all_results <- funcion_berlin(x, output = "performance/out/" )
  return( all_results$single_plots)
})
