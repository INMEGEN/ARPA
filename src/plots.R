################################################################################
#
# Plot and report functions for qPCR analysis - COVID19 detection
#by: INMEGEN Computational Genomics Dept
#Guillermo de Anda J??uregui gdeanda@inmegen.edu.mx
# and Hugo Tovar hatovar@inmegen.gob.mx
#
################################################################################


################################################################################
#required libs
################################################################################
library("cowplot")
library("ggpubr")

################################################################################
#define analysis functions here
################################################################################

##### plot_deltaRN.long 

plot_deltaRN.long <- function(tdrn_long, 
                              guide_title = "muestra", 
                              y_title = "Delta_RN"){
  #takes a long_tdrn
  #plots all curves in it, grouped by sample.id
  #(syntactic sugar)
  tdrn_long %>% 
    ggplot(mapping = aes(x = cycles, 
                         y = value, 
                         colour = as.factor(sample.id)
    )
    ) + 
    geom_line() +
    guides(color = guide_legend(title = guide_title)) +
    ylab(y_title) +
    theme_minimal()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
    							fill = NULL,
    							colour = "darkgrey",
    							size = 0.3)
          )
}


##### plot.curves
plot.curves <- function(tdrn, probes, threshold_list, qc = TRUE){
  #takes an tdrn file
  #makes plots for either qc wells
  #or non-qc wells
  
  #extracts quality control wells
  wells.ntc <- grep(pattern = "_NTC", x = colnames(tdrn))
  wells.ptc <- grep(pattern = "_PTC", x = colnames(tdrn))
  wells.exc <- grep(pattern = "_CRE", x = colnames(tdrn))
  
  ##and filter the tdrn
  if(qc == TRUE){
    qc.df <-
      tdrn %>% 
      select(c(wells.ntc, wells.ptc, wells.exc), cycles) %>% 
      pivot_deltaRN %>% 
      split_longtdrn
  }else{
    qc.df <-
      tdrn %>% 
      #select(!c(wells.ntc, wells.ptc), cycles) %>%
      select(-c(wells.ntc, wells.ptc, wells.exc), cycles) %>%
      pivot_deltaRN %>% 
      split_longtdrn
  }
  
  
  #name for iteration
  qc.samples <- unique(qc.df$sample.label)
  names(qc.samples) <- qc.samples
  
  #analyze qc
  qc.results <- 
    lapply(qc.samples, FUN = function(my_sample){
      
      sample_data <-
        qc.df %>% 
        filter(sample.label == my_sample) 
      lapply(X = probes, FUN = function(my_probe){
        
        the_curve     <- extract_curve(sample_data, probe == my_probe)
        #extract threshold
        
        color <- ifelse(unique(the_curve$probe) == "Gen RNasaP", "#e41a1c", ifelse(unique(the_curve$probe) == "Gen E", "#377eb8", "#4daf4a"))
        
        #the_threshold <- get_threshold.rg(the_curve)
        the_threshold <- threshold_list[[my_probe]]
        
        p <- 
          plot_deltaRN.long(tdrn_long = the_curve) + 
          geom_line(colour = color) +
          geom_hline(yintercept = the_threshold, linetype = 2, colour = "maroon") 
      })
      
      
    })
}


triplets <- function(curve.list){
	triplet <- lapply(seq_along(curve.list), function(i){
	plot_grid(curve.list[[i]]$N1, curve.list[[i]]$N2, curve.list[[i]]$RP, 
		labels = c("N1", "N2", "RP"),
		ncol = 1, nrow = 3,
		label_size = 11,
		hjust = 1)
	})
	triplets <- lapply(seq_along(triplet), function(i){
	annotate_figure(triplet[[i]], bottom = text_grob("Cycle", size = 10, hjust = 1),
                left = text_grob(bquote(Delta*"Rn"), size = 10, rot = 90))
	})
	names(triplets) <- names(curve.list)
	return(triplets)
}


duplas <- function(curve.list){
  triplet <- lapply(seq_along(curve.list), function(i){
  plot_grid(curve.list[[i]]$N1, curve.list[[i]]$N2, curve.list[[i]]$RP, 
    labels = c("N1", "N2", "RP"),
    ncol = 1, nrow = 3,
    label_size = 11,
    hjust = 1)
  })
  triplets <- lapply(seq_along(triplet), function(i){
  annotate_figure(triplet[[i]], bottom = text_grob("Cycle", size = 10, hjust = 1),
                left = text_grob(bquote(Delta*"Rn"), size = 10, rot = 90))
  })
  names(triplets) <- names(curve.list)
  return(triplets)
}


##### sample_curve.cdc
sample_curve.cdc <- function(tdrn, sample_id){
  #makes a curve plot with the three cdc probes for a single sample
  #and the corresponding threshold 
  
  #pivot and split 
  tdrn <- 
    tdrn %>% 
    pivot_deltaRN() %>% 
    split_longtdrn()
  
  #get thresholds 
  th_rp = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "RP"))
  th_n1 = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "N1"))
  th_n2 = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "N2"))
  
  #make plot
  
  p <- 
    suppressWarnings(
      tdrn %>% 
        filter(sample.label == sample_id) %>% 
        mutate(probe = as_factor(probe)) %>% 
        mutate(probe = fct_relevel(.f = probe, levels = c("RP", "N1", "N2"))) %>% 
        ggplot(aes(cycles, value, colour = probe)) + 
        geom_line() + 
        theme_minimal() +
        scale_color_manual(values = c("red", "green", "blue")) + 
        geom_hline(yintercept = th_rp, colour = "red", linetype = 2, alpha = 0.5) +
        geom_hline(yintercept = th_n1, colour = "green", linetype = 2, alpha = 0.5) +
        geom_hline(yintercept = th_n2, colour = "blue", linetype = 2, alpha = 0.5) +
        ggtitle(label = sample_id)
    )
  plot(p)
  
  
}





#### MODIFICATION
#### REMOVE THRESHOLDS
##### sample_curve.berlin.manual
sample_curve.berlin.manual <- function(tdrn, sample_id, probes){
  #makes a curve plot with the three cdc probes for a single sample
  #and the corresponding threshold 
  
  #pivot and split 
  tdrn <- 
    tdrn %>% 
    pivot_deltaRN() %>% 
    split_longtdrn()
  
  #get thresholds 
  #th_rp = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "RP"))
  #th_n1 = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "N1"))
  #th_n2 = try(get_probeThreshold(tdrn_long = tdrn, my_probe = "N2"))
  
  #th_1  = th_list[[probes[1]]]
  #th_2  = th_list[[probes[2]]]
  #make plot
  
  p <- 
    suppressWarnings(
      tdrn %>% 
        filter(sample.label == sample_id) %>% 
        mutate(probe = as_factor(probe)) %>% 
        mutate(probe = fct_relevel(.f = probe, levels = probes)) %>% 
        ggplot(aes(cycles, value, colour = probe)) + 
        geom_line() + 
        theme_minimal() +
        scale_color_manual(values = c("red", "blue"))+#, "blue")) + 
        #geom_hline(yintercept = th_1, colour = "red", linetype = 2, alpha = 0.5) +
        #geom_hline(yintercept = th_2, colour = "blue", linetype = 2, alpha = 0.5) +
        #geom_hline(yintercept = th_n2, colour = "blue", linetype = 2, alpha = 0.5) +
        ggtitle(label = sample_id)
    )
  plot(p)
  
  
}
