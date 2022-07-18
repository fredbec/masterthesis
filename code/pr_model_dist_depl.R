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

for (loc in locs){
  subdat <- filter(hub_data, location == loc)
  
  start_time <- Sys.time()
  allres <- mclapply(targets, function(targ)
    all_combs_ensemble(filter(hub_data, target_type == targ), moddist, nmod = nmod,
                       avail_threshold = 0),
    mc.cores = 2
  )
  names(allres) <- targets
  end_time <- Sys.time()
  run_time <- end_time - start_time
  
  lapply(names(allres), function(targ)
    saveRDS(allres[[targ]],
            here("results", "all_combs_ensemble", paste0("nmod", nmod, "_", loc, targ, ".RDS")))
  )
  
  nrow_res <- lapply(allres, nrow) |>
    unlist() |>
    mean()
  
  rm(allres)
  
  comp_times <- rbind(comp_times,
                      data.frame(nrow = nrow_res, 
                                 nmod = nmod,
                                 location = loc,
                                 comp_time = run_time))
  
  saveRDS(comp_times, here("results", "all_combs_ensemble", "comp_times_plde.RDS"))
}


