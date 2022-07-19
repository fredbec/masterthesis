library(here)
library(masterthesis)
library(data.table)
library(dplyr)
library(parallel)

source(here("code", "load_clean_data.R"))

moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

locs <- c("DE", "PL")
targets <- as.list(unique(hub_data$target_type))
nmod <- 5
window <- 5
no_weeks <- 3
no_mc.cores <- 4


#fcdates <- sort(unique(hub_data$forecast_date))[(window+1):length(unique(hub_data$forecast_date))]

fcdates <- sort(unique(hub_data$forecast_date))[(window+1):26]


make_date_list <- function(fcdates, no_weeks, window){
  fcdates <- split(sort(fcdates), 
                 ceiling(seq_along(fcdates)/no_weeks))
  
  appendates <- function(fcdate){
    mindate <- as.Date(sort(fcdate)[1])
    p_date <- mindate - window * 7
    
    
    rec_dates <- seq.Date(p_date,
                          mindate-7, by = 7)
    
    return(sort(c(rec_dates, fcdate)))
  }
  
  sections <- lapply(fcdates, function(fcdate) appendates(fcdate))
  
  return(sections)
  
}



fc_dates_list <- make_date_list(fcdates, no_weeks, window)


num_its <- ceiling(length(fc_dates_list)/no_mc.cores)
comp_times <- NULL

for (loc in locs){
  for(i in 1:length(fc_dates_list)){
    
    lwr <- (no_mc.cores * (i-1)) + 1
    upr <- no_mc.cores * i
    curr_fc_dates_list <- fc_dates_list[lwr:upr]
    
    start_time <- Sys.time()
    allres <- mclapply(curr_fc_dates_list, function(fcdates)
      all_combs_ensemble(filter(hub_data, forecast_date %in% fcdates), 
                         moddist, nmod = nmod,
                         avail_threshold = 0),
      mc.cores = no_mc.cores
    )
    names(allres) <- 1:length(fc_dates_list)
    end_time <- Sys.time()
    run_time <- end_time - start_time
    
    
    lapply(names(allres), function(num)
      saveRDS(allres[[num]],
              here("results", "all_combs_ensemble", paste0("nmod", nmod, "_", loc,"_set",num, ".RDS")))
    )
    
    nrow_res <- lapply(allres, nrow) |>
      unlist() |>
      mean()
    
    rm(allres)
    
    comp_times <- rbind(comp_times,
                        data.frame(nrow = nrow_res, 
                                   nmod = nmod,
                                   location = loc,
                                   comp_time = run_time,
                                   set = i))
    
    saveRDS(comp_times, here("results", "all_combs_ensemble", "comp_times_plde.RDS"))
    
  }
}
  