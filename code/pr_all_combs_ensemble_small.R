library(here)
library(masterthesis)
library(data.table)
library(dplyr)
library(parallel)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

#load model distance dataframe
moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

#load specs
#load locs as list for mclapply (distribute locs across cpus)
locs <- as.list(specs$all_combs_ensemble_small_countries)
nmods <- specs$all_combs_ensemble_small_nmod
window <- specs$all_combs_ensemble_window
init_weeks <- specs$all_combs_ensemble_init_weeks

no_mc.cores <- length(specs$all_combs_ensemble_small_countries)


#filter out countries that are not in list of small countries
hub_data <- hub_data |>
  filter(location %in% unlist(locs))

#init result container for computation times
comp_times <- NULL

for(nmod in nmods){
  #record computation time
  start_time <- Sys.time()
  
  #produce results
  allres <- mclapply(locs, function(loc)
    
    all_combs_ensemble(filter(hub_data, location == loc),
                       model_dist = moddist, 
                       avail_threshold = avail_threshold,
                       nmod = nmod, window = window,
                       init_weeks = init_weeks),
    mc.cores = no_mc.cores
  )
  #name result list
  names(allres) <- locs
  
  #get computation time
  end_time <- Sys.time()
  run_time <- end_time - start_time

  #save result dataframes, by country
  lapply(names(allres), function(loc)
         saveRDS(allres[[loc]],
          here("results", "all_combs_ensemble", 
               paste0("nmod", nmod, "_", loc, ".RDS")))
         )
  
  #avg number of rows in result (for comp_times)
  nrow_res <- lapply(allres, nrow) |>
    unlist() |>
    mean()
  
  #remove to save memory
  rm(allres)
  
  #append to computation time results
  comp_times <- rbind(comp_times,
                      data.frame(nrow = nrow_res, 
                                 nmod = nmod,
                                 comp_time = run_time))
  
  #could also do this outside of the loop, 
  #but this way get intermediary results in case of crash
  saveRDS(comp_times, 
          here("results", "all_combs_ensemble", "comp_times_smallcountries.RDS"))
}