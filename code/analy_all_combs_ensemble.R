library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

#test_data <- hub_data |> filter(location == "DE", target_type == "Cases",
#                                forecast_date < "2021-05-01")


mymoddist <- readRDS(here("results", "pairwise_model_dists.RDS"))

devtools::load_all()
#mymoddist <- model_dist_by_fc_date(test_data, 0.3, 0.01, cramers_dist)

#start_time <- Sys.time()
#myres <- 

myres <- all_combs_ensemble2(hub_data, mymoddist, avail_threshold = 0)
#myres_wohor <- myres
#end_time <- Sys.time()
#end_time - start_time

saveRDS(myres, here("results", "all_combs_ensemble_server_fullrun_byhor.RDS"))

#myres2 <- myres |> group_by(location, target_type, inc_models , model, quantile, forecast_date) |>
#  summarise(mean_hist_dist = mean(hist_dist_mean)) |>
#  ungroup() |>
#  data.frame() |>
#  data.table()

#check2 <- check2 |>
#  data.frame() |>
#  data.table()

#mylist <- myres |> 
#  data.table() |>
#  split(by = "horizon")


#mylist2 <- lapply(mylist, function(dat)
#  dat |>
#    group_by(model1, model2) |>
#    summarise(avgdist = mean(dist, na.rm = TRUE)) |>
#    mutate(avgdist = ifelse(is.nan(avgdist), NA, avgdist)) |>
#    model_dists_to_mat()
#)

#ens_combs <- c("itwm-dSEIR", "LANL-GrowthRate", "MUNI-ARIMA") |>
#  combn(2) |> t()

#mylist <- list()
#for (i in 1:4){
#  mylist[[i]] <- rep(NA, 3)
#}

#byhor <- lapply(mylist2, function(mat)
#  apply(ens_combs, 1, 
#        function(x) mat[x[1], x[2]]))



#mean_hist_dist <- byhor |>
#  sapply(function(x) c(mymean = ifelse(is.nan(mean(x, na.rm = TRUE)), NA, mean(x, na.rm = TRUE)),
#         mysd = sd(x),
 #        hist_pairs_avail = 3 - sum(is.na(x)))) |>
#  t() |>
#  data.table() |>
#  mutate(horizon = 1:4)
  
#save how many models available


#byhor_df <- data.frame(
#  horizon = 1:4,
#  hist_dist = mean_hist_dist
#)


#check1 <- myres |>
#  group_by(model1, model2) |>
#  summarise(distavg = mean(dist, na.rm = TRUE)) |>
#  mutate(distavg = 
#           ifelse(is.nan(distavg), NA, distavg)) |>
#  model_dists_to_mat()

#check1_avg <- apply(ens_combs, 1, 
#                    function(x) check1[x[1], x[2]])



###################################

#start_time <- Sys.time()
#myres3 <- all_combs_ensemble3(test_data, avail_threshold = 0)
#end_time <- Sys.time()
#end_time - start_time


#check1 <- myres2 |> mutate(from2 = 1)
#check2 <- myres3[[1]] |> mutate(from3 = 1)

#check2 |> filter(inc_models == "DSMPG-bayes;FIAS_FZJ-Epi1Ger;epiforecasts-EpiExpert_Rt")

#checkall <- full_join(check1, check2, by = c("model", "quantile", "horizon", "inc_models", "forecast_date"))

#saveRDS(myres2, here("results", "all_combs_ensemble_server_ens_branch2_test.RDS"))
#saveRDS(myres3[[1]], here("results", "all_combs_ensemble_server_ens_branch3.RDS"))


#before doing main run
#   decide on avail_threshold
#   how to figure out point a. 
#   what about avail_overlap_threshold? --> how to handle NAs
#   

#myres7 <- myres
#all.equal(myres7, myres)
