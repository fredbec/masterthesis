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
locs <- unique(hub_data$location)
targets <- unique(hub_data$target_type)

comp_times <- NULL

for (loc in locs){
  print(loc)
  
  #Deaths part 1
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Deaths", 
           forecast_date <= "2021-06-21")
  
  start_time <- Sys.time()
  res_de <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_de, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Deaths_part1.RDS")))
  rm(res_de)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Deaths_p1", 
                                 comp_time = run_time))
  
  
  #Deaths part 2
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Deaths",
           forecast_date >= "2021-05-24" & 
             forecast_date <= "2021-10-11")
  
  start_time <- Sys.time()
  res_de <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_de, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Deaths_part2.RDS")))
  rm(res_de)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Deaths_p2", 
                                 comp_time = run_time))
  
  #Deaths part 3
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Deaths",
           forecast_date >= "2021-09-13")
  
  start_time <- Sys.time()
  res_de <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_de, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Deaths_part3.RDS")))
  rm(res_de)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Deaths_p3", 
                                 comp_time = run_time))
  
  
  #Cases part 1
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Cases",
           forecast_date <= "2021-06-21")
  
  start_time <- Sys.time()
  res_ca <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_ca, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Cases_part1.RDS")))
  rm(res_ca)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Cases_p1", 
                                 comp_time = run_time))
  
  #Cases part 2
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Cases",
           forecast_date >= "2021-05-24" & 
             forecast_date <= "2021-10-11")
  
  start_time <- Sys.time()
  res_ca <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_ca, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Cases_part2.RDS")))
  rm(res_ca)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Cases_p2", 
                                 comp_time = run_time))
  
  #Cases part 3
  subdat <- hub_data |>
    filter(location == loc,
           target_type == "Cases",
           forecast_date >= "2021-09-13")
  
  start_time <- Sys.time()
  res_ca <- all_combs_ensemble(
    subdat, moddist, nmod = 5, avail_threshold = 0)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  print(run_time)
  saveRDS(res_ca, 
          here("results", "all_combs_ensemble", paste0("nmod5_", loc, "_Cases_part3.RDS")))
  rm(res_ca)
  comp_times <- rbind(comp_times,
                      data.frame(location = loc,
                                 target_type = "Cases_p3", 
                                 comp_time = run_time))
  
}
saveRDS(comp_times,
        here("results", "all_combs_ensemble", "comp_times.RDS"))

#print("try to save all")
#saveRDS(res_nmod4, here("results", "all_combs_ensemble", "nmod4DEsingle.RDS"))

#print("or is the problem here")
#res_list4 <- split(res_nmod4, by = "location")
#saveRDS(res_list4[["DE"]], here("results", "all_combs_ensemble", "nmod4DEsingle2.RDS"))



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