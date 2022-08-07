library(here)
library(masterthesis)
library(data.table)
library(dplyr)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

avail_threshold <- specs$model_distance_avail_threshold
avail_overlap_threshold <- specs$model_distance_avail_overlap_threshold
dist_fun <- specs$model_distance_dist_fun



###########compute pairwise model distances###############
pairwise_model_dist <- model_dist_by_fc_date(
  hub_data, avail_threshold, avail_overlap_threshold, dist_fun)

pairwise_model_dist_avghor <- pairwise_model_dist |>
  group_by(model1, model2, forecast_date, location, target_type) |>
  summarise(avg_dist = mean(dist))


pairwise_model_dist <- left_join(
  pairwise_model_dist, 
  pairwise_model_dist_avghor,
  by = c("model1", "model2", "forecast_date", 
         "location", "target_type")
)


saveRDS(pairwise_model_dist, here("results", "pairwise_model_dists.RDS"))