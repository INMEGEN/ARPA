################################################################################
#
#functions for qPCR analysis - COVID19 detection
#using function fittings
#by: INMEGEN Computational Genomics Dept
#Guillermo de Anda Jauregui gdeanda@inmegen.edu.mx
#
################################################################################

library(tidyverse)
library(growthrates)


adjust_sigmoid <- function(curva, resolucion = 0.001){
  
  ### try to adjust sigmoid with nls
  fit_try <- try(fit <- nls(value ~ SSlogis(cycles, Asym, xmid, scal),
                            data = curva), silent = T)
  
  if(class(fit_try)!= "try-error"){
    #if fit_try worked:
    
    ##predict with high resolution 
    mis_ciclos = pull(.data = curva, cycles)
    ciclos_int = seq(from =min(mis_ciclos), 
                     to=max(mis_ciclos), 
                     by = resolucion)
    
    predicted <- 
      predict(object = fit, 
              newdata = data.frame(cycles = ciclos_int
              )
      )
    
    df <- data.frame(cycles = ciclos_int, 
                     value  = predicted
    )
      #check that growth is positive 
    df.check <-
      df %>% 
      mutate(increasing = value >= lag(value))
    
    
    if(all(df.check$increasing[-1] == T)){ #first one has no lag
      #return high res data frame 
      if(100*first(df$value) > last(df$value)){
        return("not_adjusted")
      }else{
          return(df)
      }
    } #this is the best case scenario! 
      
    else{
      #fail with not_adjusted
      message("decreasing logistic")
      return("not_adjusted")
      }
        
  }else{
    
    #if fit_try failed
    ### try to adjust with growth rates 
    message("logistic fit failed; trying generalized logistic fit")
    #####define some parameters
    pars <- c(y0 = 0.01, mumax = 0.2, K = 0.1)
    
    my_y = curva %>% pull(value)
    my_time = curva %>% pull(cycles)
    my_r = growthrates::fit_growthmodel(FUN = grow_logistic, 
                                      p = pars, 
                                      y = my_y, 
                                      time = my_time)
    
    
    ### Rsquare greater than 0.9?
    if(my_r@rsquared > 0.9){
      #return("rr es bueno")
      df = my_r@obs %>% 
        rename(cycles = time, value = y)
      
      #check that growth is positive by doing a lazy exponential adjustment
      bad_exp_adj <-
      try(
      nls(value ~ a*exp(r*cycles), 
          data = df, 
          start = list(a = 0.5, r = 0.2)) %>%
        predict(new_data = data.frame(cycles = 1:45)) %>% 
        data.frame(cycles = 1:45, value = .)
      )
      
      if(class(bad_exp_adj)=="try-error"){
        message("last try: see if first value is less than ten times first value")
        
        if(100*first(df$value) < last(df$value)){
          
          message("last is at least 10 times greater than first")
          return(df)
          
        }else{
          message("this is a really extreme case, please check manually")
          return("not_adjusted")
        }
      }
      
      df.check <- 
        bad_exp_adj %>% 
        mutate(increasing = value >= lag(value))
        
      if(all(df.check$increasing[-1] == T)){ #first one has no lag
        #return high res data frame 
        if(100*first(df$value) > last(df$value)){
          return("not_adjusted")
        }else{
          return(df) #this is the second best escenario 
        }
      } 
      
      else{
        #fail with not_adjusted
        message("no plateau AND decreasing logistic")
        return("not_adjusted")
      }
      
    }else{
      return("not_adjusted")
    }
  }
}


###
adjust_sigmoid_strict <- function(curva, resolucion = 0.001){
 ### takes a curve,
 
  ### checks if it is sigmoid 
  ajuste <-
  tryCatch(
    {
    fit <- nls(value ~ SSlogis(cycles, Asym, xmid, scal),
               data = curva)
    
    
    ### returns adjusted curve
    mis_ciclos = pull(.data = curva, cycles)
    ciclos_int = seq(from =min(mis_ciclos), 
                     to=max(mis_ciclos), 
                     by = resolucion)
    
     predicted <- 
       predict(object = fit, 
               newdata = data.frame(cycles = ciclos_int
               )
       )
    
    ### as a data frame 
    
     df <- data.frame(cycles = ciclos_int, 
                      value  = predicted
                        )
     
     ### check that logistic is growing
     df.check <-
     df %>% 
       mutate(increasing = value >= lag(value))
     
     
     if(all(df.check$increasing[-1] == T)){
       return(df)}
       else{
         message("decreasing logistic")
         return("not_adjusted")
       }
     },
    ### else returns "not_adjusted"
    error =function(cond){
      message(cond)
      #message("well does not fit logistic regression")
      return("not_adjusted")}
  )
  return(ajuste)
}
 

analyze_sample_x <- function(tdrn_sample, probes, threshold_list){
  

  list_adjusted <- lapply(X = probes, FUN = function(i){
      
      
      #get threshold for probe
      threshold <- threshold_list[[i]]
      
      #get curve for probe
      curva <- extract_curve(tdrn_long = tdrn_sample, probe == i)
      
      #make adjustment
      my_adjustment <- adjust_sigmoid(curva = curva)
      
      #check where curve crosses
      if(is.character(my_adjustment)){
        Ct <- 99
      }else{
        Ct <-
          my_adjustment %>% 
          filter(value >= threshold) %>% 
          pull(cycles) %>% min
      }
    }) %>% bind_rows() 
    
  return(list_adjusted)
}

analyze_sample <- analyze_sample_x
