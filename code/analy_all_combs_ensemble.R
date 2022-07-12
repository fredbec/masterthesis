library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

test_data <- hub_data |> filter(location == "DE", target_type == "Cases",
                                forecast_date < "2021-05-01")


mymoddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

devtools::load_all()
start_time <- Sys.time()
myres <- all_combs_ensemble2(test_data, mymoddist, nmod = 4, avail_threshold = 0)
end_time <- Sys.time()
end_time - start_time

check1 <- readRDS(here("results", "all_combs_ensemble_server_fullrun_byhor.RDS"))
check2 <- readRDS(here("results", "all_combs_ensemble_server_fullrun.RDS"))


saveRDS(myres, here("results", "all_combs_ensemble_server_fullrun_byhor.RDS"))


###################################

#before doing main run
#   decide on avail_threshold
#   how to figure out point a. 
#   what about avail_overlap_threshold? --> how to handle NAs
#   