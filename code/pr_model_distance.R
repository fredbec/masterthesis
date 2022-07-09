library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

pairwise_model_dist <- model_dist_by_fc_date(hub_data, 0, 0, cramers_dist)

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


dat <- pairwise_model_dist |> 
  filter(location == "DE", target_type == "Cases",
         forecast_date < "2021-05-10") |>
  group_by(model1, model2) |>
  summarise(distavg = mean(dist, na.rm = TRUE)) |>
  mutate(distavg = ifelse(is.nan(distavg), NA, distavg)) |>
  model_dists_to_mat()

