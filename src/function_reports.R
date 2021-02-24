
source("src/reports.R")

function_reports <- function(results_list, input_eds, output){
  
  ###### MAKING HTML REPORTS
  
  try(make_reports(plot_list = results_list$single_plots, 
                   result_table = results_list$qc_results$qc.values[,1:3], 
                   input = input_eds,
                   outdir = output, 
                   qc_results = results_list$qc_results$QC,
                   qc = F))
  
  try(make_reports(plot_list = results_list$single_plots, 
                   result_table = results_list$qc_results$qc.values[,1:3], 
                   input = input_eds,
                   outdir = output, 
                   qc_results = results_list$qc_results$QC,
                   qc = T))
  
}