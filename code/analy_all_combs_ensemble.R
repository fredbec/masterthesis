library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

test_data <- hub_data |> filter(location == "DE", 
                                target_type == "Cases",
                                forecast_date < "2021-05-01")

devtools::load_all()
mymoddist <- model_dist_by_fc_date(test_data, 0.3, 0.01, cramers_dist)

#change avail
#devtools::load_all()
#start_time <- Sys.time()
myres <- all_combs_ensemble(test_data, mymoddist, avail_threshold = 0.3)
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
