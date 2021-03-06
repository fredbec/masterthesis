library(here)
library(masterthesis)
library(data.table)
library(dplyr)
library(parallel)

source(here("code", "load_clean_data.R"))

moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

loc <- "DE"
window <- specs$all_combs_ensemble_window
init_weeks <- specs$all_combs_ensemble_init_weeks
avail_threshold <- specs$all_combs_ensemble_avail_threshold

nmod <- 8
no_weeks <- 1

#subset hub_data for only DE
hub_data <- filter(hub_data, location == loc)

#scheduler of cores for mclapply
no_mc.cores = c(2, rep(1, 12), rep(2, 5), rep(3, 6))
#number of iterations needed to go over list
num_its = length(no_mc.cores)

#this just gets the forecast dates that end up in the result dataframe
#i.e. excluding the (5) init_weeks
fcdates <- sort(unique(hub_data$forecast_date))[(window+1):length(unique(hub_data$forecast_date))]



#small helper function to get list of dates
#this splits up fcdates into a list with sets of `no_weeks` dates
#and attaches the previous `window` week dates (this is just
#because the way the `all_combs_ensemble`-function works,
#it always needs the data to have the init weeks as well)
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


#init result container
comp_times <- NULL


lwr <- 1
for(i in 1:num_its){
  
  #get current `no_mc.cores` elements from fc_dates_list
  num_sets <- no_mc.cores[i]
  upr <- (lwr + num_sets) - 1
  curr_fc_dates_list <- fc_dates_list[lwr:upr]
  
  #record computation time
  start_time <- Sys.time()
  #make results, with mclapply
  allres <- mclapply(curr_fc_dates_list, function(fcdates)
    
    all_combs_ensemble(filter(hub_data,
                              forecast_date %in% fcdates),
                       model_dist = moddist,
                       avail_threshold = avail_threshold,
                       nmod = nmod, window = window,
                       init_weeks = init_weeks),
    mc.cores = no_mc.cores[i]
  )
  
  #name result list
  names(allres) <- lwr:upr
  end_time <- Sys.time()
  run_time <- end_time - start_time
  
  #save results, by set
  lapply(names(allres), function(num)
    saveRDS(allres[[num]],
            here("results", "all_combs_ensemble", 
                 paste0("nmod", nmod, "_", loc,"_set",num, ".RDS")))
  )
  
  #get avg number of rows (for comp_times)
  nrow_res <- lapply(allres, nrow) |>
    unlist() |>
    mean()
  
  #save memory
  rm(allres)
  
  comp_times <- rbind(comp_times,
                      data.frame(nrow = nrow_res, 
                                 nmod = nmod,
                                 location = loc,
                                 comp_time = run_time,
                                 set = i))
  
  #save intermediary results in case of crash
  saveRDS(comp_times, here("results", "all_combs_ensemble", "comp_times_de.RDS"))
  
  #prep for next iteration
  lwr <- upr + 1
}

