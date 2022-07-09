library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

test_data <- hub_data |> filter(location == "DE", 
                                target_type == "Cases",
                                forecast_date < "2021-05-08")

mymoddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

devtools::load_all()
#mymoddist <- model_dist_by_fc_date(test_data, 0.3, 0.01, cramers_dist)

#change avail
#devtools::load_all()
start_time <- Sys.time()
myres2 <- all_combs_ensemble2(test_data, mymoddist, avail_threshold = 0)
end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()
myres3 <- all_combs_ensemble3(test_data, avail_threshold = 0)
end_time <- Sys.time()
end_time - start_time


check1 <- myres2 |> mutate(from2 = 1)
check2 <- myres3[[1]] |> mutate(from3 = 1)

check2 |> filter(inc_models == "DSMPG-bayes;FIAS_FZJ-Epi1Ger;epiforecasts-EpiExpert_Rt")

checkall <- full_join(check1, check2, by = c("model", "quantile", "horizon", "inc_models", "forecast_date"))

saveRDS(myres[[1]], here("results", "all_combs_ensemble_local_ens.RDS"))
saveRDS(myres[[2]], here("results", "all_combs_ensemble_local_dists.RDS"))


#before doing main run
#   decide on avail_threshold
#   how to figure out point a. 
#   what about avail_overlap_threshold? --> how to handle NAs
#   

#myres7 <- myres
#all.equal(myres7, myres)
