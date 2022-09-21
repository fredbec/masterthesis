library(here)
library(masterthesis)
library(scoringutils)
library(data.table)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

#will often need model type info after scoring
#but this column could in theory not work with scoringutils
model_types <- hub_data |>
  select(model, model_type) |>
  distinct()


#also need target end date after scoring
tg_end_map <- hub_data |> 
  select(forecast_date, horizon, target_end_date) |>
  distinct()

###############################Scoring##########################
#score all models
score_all_mods <- hub_data |>
  select(specs$su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon")) |>
  left_join(model_types, by = c("model"))

#Version with relative skill
score_all_mods_with_relwis <- score_all_mods |>
  select(-model_type) |>
  summarise_scores(by = c("model", "target_type", "forecast_date",
                          "location"),
                   relative_skill = TRUE,
                   baseline = "EuroCOVIDhub-baseline")

#Version with relative skill, by horizon
score_all_mods_with_relwis_avg <- score_all_mods |>
  select(-model_type) |>
  summarise_scores(by = c("model", "target_type",
                          "location", "horizon"),
                   relative_skill = TRUE,
                   baseline = "EuroCOVIDhub-baseline")


#Score unweighted mean and median ensemble
mean_ens <- hub_data |>
  make_ensemble(summary_function = mean) |>
  filter(model == "mean_ensemble")

median_ens <- hub_data |>
  make_ensemble(summary_function = median) |>
  filter(model == "median_ensemble")

mean_ens_scores <- mean_ens |>
  select(specs$su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))

median_ens_scores <- median_ens |>
  select(specs$su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))


#make and score model type ensembles
stat_ens <- hub_data |>
  dplyr::filter(model_type == "statistical") |>
  make_ensemble(summary_function = median,
                model_name = "stat_ensemble") |>
  dplyr::filter(model == "stat_ensemble")
mech_ens <- hub_data |>
  dplyr::filter(model_type == "mechanistic") |>
  make_ensemble(summary_function = median,
                model_name = "mech_ensemble") |>
  dplyr::filter(model == "mech_ensemble")
semi_ens <- hub_data |>
  dplyr::filter(model_type == "semi") |>
  make_ensemble(summary_function = median,
                model_name = "semi_ensemble") |>
  dplyr::filter(model == "semi_ensemble")

#rbind all ensembles
model_type_ens <- stat_ens |>
  rbind(mech_ens) |>
  rbind(semi_ens)

#score all ensembles
model_type_ens_scores <- model_type_ens |>
  select(specs$su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))


data.table::fwrite(score_all_mods, here("score_data", "score_all_models.csv"))
data.table::fwrite(score_all_mods_with_relwis, here("score_data", "score_all_mods_with_relwis.csv"))
data.table::fwrite(score_all_mods_with_relwis_avg, here("score_data", "score_all_mods_with_relwis_avg.csv"))
data.table::fwrite(mean_ens_scores, here("score_data", "mean_ensemble_scores.csv"))
data.table::fwrite(median_ens_scores, here("score_data", "median_ensemble_scores.csv"))
data.table::fwrite(model_type_ens_scores, here("score_data", "model_type_ensemble_scores.csv"))

data.table::fwrite(mean_ens, here("ensembles", "mean_ensemble.csv"))
data.table::fwrite(median_ens, here("ensembles", "median_ensemble.csv"))
data.table::fwrite(model_type_ens, here("ensembles", "model_type_ensemble.csv"))



