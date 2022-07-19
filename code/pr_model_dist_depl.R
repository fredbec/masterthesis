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
avail_threshold <- 0


#fcdates <- sort(unique(hub_data$forecast_date))[(window+1):length(unique(hub_data$forecast_date))]

fcdates <- sort(unique(hub_data$forecast_date))[(window+1):26]


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


#number of iterations needed to go over list
num_its <- ceiling(length(fc_dates_list)/no_mc.cores)

#init result container
comp_times <- NULL


#loop over locations
for (loc in locs){
  print(loc)
  for(i in 1:num_its){
    
    #get `no_mc.cores` elements from list
    lwr <- (no_mc.cores * (i-1)) + 1
    upr <- ifelse(no_mc.cores * i > length(fc_dates_list),
                  length(fc_dates_list), 
                  no_mc.cores * i)
    curr_fc_dates_list <- fc_dates_list[lwr:upr]

    #make results, with mclapply
    start_time <- Sys.time()
    allres <- mclapply(curr_fc_dates_list, function(fcdates)
      all_combs_ensemble(
        filter(hub_data, location == loc, forecast_date %in% fcdates),
        moddist, nmod = nmod,
        avail_threshold = avail_threshold),
      mc.cores = no_mc.cores
      )
    names(allres) <- lwr:upr
    end_time <- Sys.time()
    run_time <- end_time - start_time
    
    
    lapply(names(allres), function(num)
      saveRDS(allres[[num]],
              here("results", "all_combs_ensemble", paste0("nmod", nmod, "_", loc,"_set",num, ".RDS")))
    )
    
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
    
    saveRDS(comp_times, here("results", "all_combs_ensemble", "comp_times_plde.RDS"))
  }
}
  