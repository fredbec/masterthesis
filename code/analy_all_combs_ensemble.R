library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))


#change avail
#devtools::load_all()
#start_time <- Sys.time()
myres <- all_combs_ensemble(hub_data, avail_threshold = 0.3)
#end_time <- Sys.time()
#end_time - start_time


saveRDS(myres[[1]], here("results", "all_combs_ensemble_local_ens.RDS"))
saveRDS(myres[[2]], here("results", "all_combs_ensemble_local_dists.RDS"))


#before doing main run
#   decide on avail_threshold
#   how to figure out point a. 
#   what about avail_overlap_threshold? --> how to handle NAs
#   

#myres7 <- myres
#all.equal(myres7, myres)
