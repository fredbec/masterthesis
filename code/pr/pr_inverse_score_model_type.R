library(masterthesis)
library(here)
library(spatstat.geom) #for weighted.median function
source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

devtools::load_all()

window <- 4
exp_smooth <- specs$inverse_score_ensemble_exp_smooth


fc_dates <- sort(unique(hub_data$forecast_date))[-(1:window)] |> 
  as.list()

##############################VERSION WITH MODEL_TYPE ENSEMBLES#######################
#load model type ensembles and scores
model_type_ens <- 
  data.table::fread(here("ensembles", "model_type_ensemble.csv"))
model_type_ens_scores <- 
  data.table::fread(here("score_data", "model_type_ensemble_scores.csv"))

###Inverse score no exp_smooth
invscore_weights <- lapply(fc_dates, function(fcdat)
  inverse_score_weights(data = model_type_ens, 
                        score_data = model_type_ens_scores,
                        su_cols = NULL, 
                        fc_date = fcdat, 
                        exp_smooth = NULL, 
                        window = window)) |>
  rbindlist()

###Inverse score with exp_smooth
invscore_weights_expsmooth <- lapply(fc_dates, function(fcdat)
  inverse_score_weights(data = model_type_ens, 
                        score_data = model_type_ens_scores,
                        su_cols = NULL, 
                        fc_date = fcdat, 
                        exp_smooth = exp_smooth, 
                        window = window)) |>
  rbindlist()

###Save weights
data.table::fwrite(invscore_weights, 
                   here("ensembles", "weights", "invscore_weights.csv"))
data.table::fwrite(invscore_weights_expsmooth, 
                   here("ensembles", "weights", "invscore_weights_expsmooth.csv"))

###Make ensembles
invscore_weights_ensemble <- model_type_ens |>
  #remove forecast dates from first window (no weights yet)
  filter(forecast_date >= fc_dates[[1]]) |>
  left_join(invscore_weights, 
            by = c("model", "location", "target_type", "forecast_date")) |>
  #set weights to 0 if NA (only affects statistic ensemble at 3 dates because of onset)
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(summary_function = weighted.mean) |>
  #remove all other models
  filter(model == "weighted.mean_ensemble")

invscore_weights_expsmooth_ensemble <- model_type_ens |>
  #remove forecast dates from first window (no weights yet)
  filter(forecast_date >= fc_dates[[1]]) |>
  left_join(invscore_weights_expsmooth, 
            by = c("model", "location", "target_type", "forecast_date")) |>
  #set weights to 0 if NA (only affects statistic ensemble at 3 dates because of onset)
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(summary_function = weighted.mean)|>
  #remove all other models
  filter(model == "weighted.mean_ensemble")


####"Baseline" (ensemble with only dominant model types)
median_ensemble_dom_model_types <- hub_data |>
  filter(model_type %in% c("statistical", "mechanistic", "semi")) |>
  make_ensemble(summary_function = median) |>
  #remove all other models
  filter(model == "median_ensemble")

mean_ensemble_dom_model_types <- hub_data |>
  filter(model_type %in% c("statistical", "mechanistic", "semi")) |>
  make_ensemble(summary_function = mean) |>
  #remove all other models
  filter(model == "mean_ensemble")

####
data.table::fwrite(invscore_weights_ensemble, 
                   here("ensembles", "invscore_weights_ensemble.csv"))
data.table::fwrite(invscore_weights_expsmooth_ensemble, 
                   here("ensembles", "invscore_weights_expsmooth_ensemble.csv"))
data.table::fwrite(median_ensemble_dom_model_types, 
                   here("ensembles", "median_ensemble_dom_model_types.csv"))
data.table::fwrite(mean_ensemble_dom_model_types, 
                   here("ensembles", "mean_ensemble_dom_model_types.csv"))




####################VERSION WITH MODEL_TYPE AS SCORE LEVEL#######################
#load model scores
model_type_scores <- 
  data.table::fread(here("score_data", "score_all_models.csv")) |>
  filter(model_type %in% c("mechanistic", "semi", "statistical"))

model_type_data <- hub_data |>
  filter(model_type %in% c("mechanistic", "semi", "statistical"))


###"Raw" inverse score weights (not adjusted yet)
invscore_weights_model_level <- lapply(fc_dates, function(fcdat)
  inverse_score_weights(data = model_type_data, 
                        score_data = model_type_scores,
                        su_cols = NULL, 
                        fc_date = fcdat, 
                        at_level = "model_type", #this is the relevant difference to before
                        exp_smooth = NULL, 
                        window = window)) |>
  rbindlist()



###"Raw" inverse score weights (not adjusted yet)
invscore_weights_model_level_expsmooth <- lapply(fc_dates, function(fcdat)
  inverse_score_weights(data = model_type_data, 
                        score_data = model_type_scores,
                        su_cols = NULL, 
                        fc_date = fcdat, 
                        at_level = "model_type", #this is the relevant difference to before
                        exp_smooth = exp_smooth, 
                        window = window)) |>
  rbindlist()


model_type_data_expsmooth <- model_type_data |>
  left_join(invscore_weights_model_level_expsmooth, 
            by = c("model_type", "location", "target_type", "forecast_date")) |>
  filter(forecast_date >= fc_dates[[1]]) |>
  adjust_for_size() #from utils
model_type_data <- model_type_data |>
  left_join(invscore_weights_model_level, 
            by = c("model_type", "location", "target_type", "forecast_date")) |>
  filter(forecast_date >= fc_dates[[1]]) |>
  adjust_for_size() #from utils

#update inverse score weights
invscore_weights_model_level <- model_type_data |> 
  select(model, target_type, location, forecast_date, weights) |> 
  distinct() |> 
  arrange(forecast_date, location, target_type, model)
invscore_weights_model_level_expsmooth <- model_type_data_expsmooth |> 
  select(model, target_type, location, forecast_date, weights) |> 
  distinct() |> 
  arrange(forecast_date, location, target_type, model)


##Save weights
data.table::fwrite(invscore_weights_model_level, 
                   here("ensembles", "weights", "invscore_weights_model_level.csv"))
data.table::fwrite(invscore_weights_model_level_expsmooth, 
                   here("ensembles", "weights", "invscore_weights_model_level_expsmooth.csv"))


#Make ensembles
invscore_weights_model_level_median_ensemble <- model_type_data |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.median,
                model_name = "weighted.median_ensemble") |>
  filter(model == "weighted.median_ensemble")
invscore_weights_model_level_mean_ensemble <- model_type_data |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.mean,
                model_name = "weighted.mean_ensemble") |>
  filter(model == "weighted.mean_ensemble")

invscore_weights_model_level_median_ensemble_expsmooth <- model_type_data_expsmooth |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.median,
                model_name = "weighted.median_ensemble") |>
  filter(model == "weighted.median_ensemble")
invscore_weights_model_level_mean_ensemble_expsmooth <- model_type_data_expsmooth |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.mean,
                model_name = "weighted.mean_ensemble") |>
  filter(model == "weighted.mean_ensemble")



####
data.table::fwrite(invscore_weights_model_level_median_ensemble, 
                   here("ensembles", "invscore_weights_model_level_median_ensemble.csv"))
data.table::fwrite(invscore_weights_model_level_mean_ensemble, 
                   here("ensembles", "invscore_weights_model_level_mean_ensemble.csv"))
data.table::fwrite(invscore_weights_model_level_median_ensemble_expsmooth, 
                   here("ensembles", "invscore_weights_model_level_median_ensemble_expsmooth.csv"))
data.table::fwrite(invscore_weights_model_level_mean_ensemble_expsmooth, 
                   here("ensembles", "invscore_weights_model_level_mean_ensemble_expsmooth.csv"))



##Finally, make ensemble that excludes semi-mechanistics at longer horizons
semi_rem_ens <- hub_data |> 
  mutate(rem = ifelse(model_type == "semi" & horizon %in% c(3,4), 1, 0)) |>
  filter(rem == 0) |>
  select(-rem) |>
  make_ensemble(median) |>
  filter(model == "median_ensemble")


data.table::fwrite(semi_rem_ens, 
                   here("ensembles", "semi_remove_ensemble.csv"))
