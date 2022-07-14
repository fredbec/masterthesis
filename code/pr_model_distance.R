library(here)
library(masterthesis)
library(data.table)
library(dplyr)

source(here("code", "load_clean_data.R"))


###########compute pairwise model distances###############
#pairwise_model_dist <- model_dist_by_fc_date(hub_data, 0, 0, cramers_dist)

#pairwise_model_dist_avghor <- pairwise_model_dist |>
#  group_by(model1, model2, forecast_date, location, target_type) |>
#  summarise(avg_dist = mean(dist))


#pairwise_model_dist <- left_join(
#  pairwise_model_dist, 
#  pairwise_model_dist_avghor,
#  by = c("model1", "model2", "forecast_date", 
#         "location", "target_type")
#)


#saveRDS(pairwise_model_dist, here("results", "pairwise_model_dists.RDS"))



###############run all_combs_ensemble################
moddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

#nmod = 3 run
#start_time <- Sys.time()
#res_nmod3 <- all_combs_ensemble(hub_data, moddist, nmod = 3, avail_threshold = 0)
#end_time <- Sys.time()
#print("time for nmod = 3 is")
#end_time - start_time



#res_list3 <- split(res_nmod3, by = "location")
#save results
#sapply(names(res_list3), function(loc) 
#  saveRDS(res_list3[[loc]], here("results", "all_combs_ensemble", paste0("nmod3_", loc, ".RDS"))))


#nmod = 4 run
hub_data <- hub_data |>
  filter(location %in% c("DE"))
start_time <- Sys.time()
res_nmod4 <- all_combs_ensemble(hub_data, moddist, nmod = 4, avail_threshold = 0)
end_time <- Sys.time()
print("time for nmod = 4 is")
end_time - start_time

half1 <- res_nmod4 |> 
  filter(forecast_date < "2021-09-21")

half2 <- res_nmod4 |>
  filter(forecast_date >= "2021-09-21")

saveRDS(half1, here("results", "all_combs_ensemble", "nmod4DE_half1.RDS"))
saveRDS(half2, here("results", "all_combs_ensemble", "nmod4DE_half2.RDS"))

print("try to save all")
saveRDS(res_nmod4, here("results", "all_combs_ensemble", "nmod4DEsingle.RDS"))

print("or is the problem here")
res_list4 <- split(res_nmod4, by = "location")
saveRDS(res_list4[["DE"]], here("results", "all_combs_ensemble", "nmod4DEsingle2.RDS"))



#sapply(names(res_list4), function(loc) 
#  saveRDS(res_list4[[loc]], here("results", "all_combs_ensemble", paste0("nmod4_", loc, ".RDS"))))


#nmod = 5 run

#start_time <- Sys.time()
#res_nmod5 <- all_combs_ensemble(hub_data, moddist, nmod = 5, avail_threshold = 0)
#end_time <- Sys.time()
#print("time for nmod = 5 is")
#end_time - start_time

#res_list5 <- split(res_nmod5, by = "location")


#sapply(names(res_list5), function(loc) 
#  saveRDS(res_list5[[loc]], here("results", "all_combs_ensemble", paste0("nmod5_", loc, ".RDS"))))