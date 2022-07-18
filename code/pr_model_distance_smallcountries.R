library(here)
library(masterthesis)
library(data.table)
library(dplyr)

source(here("code", "load_clean_data.R"))

moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

#nmod = 4 run:
locs <- c("GB", "CZ", "FR")

hub_data <- hub_data |>
  filter(location %in% locs)

nmods <- c(3,4,5,6,7,8)

comp_times <- NULL

for(nmod in nmods){

  for (loc in locs){
    
    if(nmod == 8 & loc %in% c("FR", "GB")){
      next
    }
    
    subdat <- hub_data |>
      filter(location == loc)
    
    start_time <- Sys.time()
    res_loc <- all_combs_ensemble(subdat, moddist, nmod = nmod, avail_threshold = 0)
    end_time <- Sys.time()
    run_time <- end_time - start_time
    
    saveRDS(res_loc, 
            here("results", "all_combs_ensemble", paste0("nmod", nmod, "_", loc, ".RDS")))
    rm(res_loc)
    
    
    comp_times <- rbind(comp_times,
                        data.frame(location = loc,
                                   nmod = nmod,
                                   comp_time = run_time))
    
    saveRDS(comp_times, 
            here("results", "all_combs_ensemble", "comp_times_smallcountries.RDS"))
  
  }
  
  
}