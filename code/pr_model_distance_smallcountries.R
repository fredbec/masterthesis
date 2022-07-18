library(here)
library(masterthesis)
library(data.table)
library(dplyr)

source(here("code", "load_clean_data.R"))

moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

#nmod = 4 run:
locs <- as.list(c("GB", "CZ", "FR"))

hub_data <- hub_data |>
  filter(location %in% locs)

nmods <- c(3,4,5,6,7)

comp_times <- NULL
for(nmod in nmods){
  start_time <- Sys.time()
  allres <- mclapply(locs, function(loc)
    all_combs_ensemble(filter(hub_data, location == loc), moddist, nmod = nmod,
                       avail_threshold = 0),
    mc.cores = 3
    )
  end_time <- Sys.time()
  run_time <- end_time - start_time

  saveRDS(allres,
          here("results", "all_combs_ensemble", paste0("nmod", nmod, "_smallcountries.RDS")))
  nrow_res <- nrow(allres)
  rm(allres)
  
  comp_times <- rbind(comp_times,
                      data.frame(nrow = nrow_res, 
                                 nmod = nmod,
                                 comp_time = run_time))
}