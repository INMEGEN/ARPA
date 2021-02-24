################################################################################
#
# R code for generat reports - COVID19 eds app/COVID binnacle
# by: INMEGEN Computational Genomics Dept
# Hugo Tovar hatovar@inmegen.gob.mx and
#
################################################################################

#
#
#


################################################################################
#required libs
################################################################################
library("rmarkdown")
library("ezknitr")
library("knitr")
library("kableExtra")
######make_reports

make_reports <- function(plot_list,
                         result_table,
                         input,
                         outdir,
                         qc_results,
                         qc = FALSE) {
  plate <- stringr::str_remove(string = basename(input), pattern = ".eds")
  plate_sec <- paste0(gsub("[^[:alnum:]]", "_",
                           unique(plate[order(plate)])),
                      collapse = "-")
  reports_folder <- paste0("plate_", plate_sec, "_reports/")
  smpls <- which(!(names(plot_list) %in% c("NTC", "PTC", "CRE")))
  smpls_plots <- plot_list[smpls]
  if (qc == FALSE) {
  for (i in seq_along(smpls_plots)) {
        ezknit(file = "templates/report_sample_.Rmd",
          out_dir = paste0(outdir, "/", reports_folder),
          out_suffix = names(smpls_plots)[i],
          keep_md = FALSE,
          params = list(the_sample_is = names(smpls_plots)[i],
                        plate = plate,
                        qc_results = qc_results,
                        p = smpls_plots[[i]],
                        date = Sys.Date()
                      )
        )}
    file.copy(paste0(getwd(),"/templates/ARPA_logo.png"), paste0(outdir, "/", reports_folder))
  } else {
    my_r <- result_table[, c("sample", "gen_e", "gen_r_nasa_p")]
    ntc_plot <- plot_list[grep(pattern = "NTC", x = names(plot_list))]
    ptc_plot <- plot_list[grep(pattern = "PTC", x = names(plot_list))]
    exc_plot <- plot_list[grep(pattern = "CRE", x = names(plot_list))]
        ezknit(file = "templates/qc_report_plate_.Rmd",
          out_dir = paste0(outdir, "/", reports_folder),
          out_suffix = plate_sec,
          keep_md = FALSE,
          params = list(plate = plate,
                        qc_results = qc_results,
                        my_r = my_r,
                        ntc_p = ntc_plot,
                        ptc_p = ptc_plot,
                        exc_p = exc_plot,
                        date = Sys.Date()
                      )
            )
        file.copy(paste0(getwd(),"/templates/ARPA_logo.png"), paste0(outdir, "/", reports_folder))
        }
}
