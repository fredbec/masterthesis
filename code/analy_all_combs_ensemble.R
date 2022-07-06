library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

test_dat <- hub_data |> filter(forecast_date < as.Date("2021-06-05"),
                              location %in% c("CZ", "DE"))

#change avail
#devtools::load_all()
#start_time <- Sys.time()
myres <- all_combs_ensemble(test_dat, avail_threshold = 0.5)
#end_time <- Sys.time()
#end_time - start_time


saveRDS(myres[[1]], here("results", "all_combs_ensemble_server_ens.RDS"))
saveRDS(myres[[2]], here("results", "all_combs_ensemble_server_dists.RDS"))


#before doing main run
#   decide on avail_threshold
#   how to figure out point a. 
#   what about avail_overlap_threshold? --> how to handle NAs
#   

#myres7 <- myres
#all.equal(myres7, myres)
