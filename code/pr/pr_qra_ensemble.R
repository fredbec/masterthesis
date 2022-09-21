library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

#can check whether it improves upon any of the single ensembles
devtools::load_all()

window <- specs$inverse_score_ensemble_window

model_type_ensemble <- fread(here("ensembles", "model_type_ensemble.csv")) 

fc_dates <- sort(unique(hub_data$forecast_date))[-(1:window)] |> 
  as.list()

###compute QRA model type weights
qra_model_type_weights <- lapply(fc_dates, function(fcdat)
  qra_weights(data = model_type_ensemble, 
              fc_date = fcdat, 
              window = 4)) |>
  rbindlist()


###make ensemble based on model type mini ensembles
qra_model_type_ensemble <- model_type_ensemble |>
  left_join(qra_model_type_weights, 
            by = c("location", "target_type", "forecast_date", "model")) |>
  filter(forecast_date >= fc_dates[[1]]) |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.mean, model_name = "weighted.mean_ensemble") |>
  filter(model == "weighted.mean_ensemble")


data.table::fwrite(qra_model_type_weights, 
                   file = here("ensembles", "weights", "qra_model_type_weights.csv"))
data.table::fwrite(qra_model_type_ensemble, 
                   file = here("ensembles", "qra_model_type_ensemble.csv"))



#################Adjust weights####################
#compute weights for an equally weighted ensemble 
weight_adjuster <- hub_data |>
  filter(!model_type %in% c("other", "ensemble", "baseline")) |>
  select(location, target_type, forecast_date, model, model_type) |>
  distinct() |>
  group_by(location, target_type, forecast_date, model_type) |>
  #get number of models available for each model type
  summarise(count = n()) |>
  ungroup() |>
  group_by(location, target_type, forecast_date) |>
  mutate(ovr_count = sum(count)) |>
  mutate(prop_mt = count/ovr_count) |> #proportion of each model type
  #this is simply for merging with other data
  mutate(model_type = 
           factor(model_type,
                  levels = c("mechanistic", "semi", "statistical"),
                  labels = c("mech_ensemble", "semi_ensemble", "stat_ensemble"))) |>
  mutate(model = as.character(model_type)) |>
  select(-c(count, ovr_count, model_type)) |>
  filter(forecast_date >= "2021-04-12") #date from which estimations are available


#compute adjusted weights
qraweights_adj <- fread(here("ensembles", "weights", "qra_model_type_weights.csv")) |>
  full_join(weight_adjuster, 
            by = c("location", "target_type", "forecast_date", "model")) |> #full join to also give statistica in early periods some weight
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  mutate(adj_weights = (weights + prop_mt)/2) |> #compute adjusted weights
  select(-c(weights, prop_mt)) |>
  rename(weights = adj_weights)


#make ensemble based on adjusted weights
qra_model_type_ensemble_adj <- model_type_ensemble |>
  left_join(qraweights_adj, 
            by = c("location", "target_type", "forecast_date", "model")) |>
  filter(forecast_date >= fc_dates[[1]]) |>
  mutate(weights = ifelse(is.na(weights), 0, weights)) |>
  make_ensemble(weighted.mean, model_name = "weighted.mean_ensemble") |>
  filter(model == "weighted.mean_ensemble")


data.table::fwrite(qraweights_adj, 
                   file = here("ensembles", "weights", "qra_model_type_adj_weights.csv"))
data.table::fwrite(qra_model_type_ensemble_adj, 
                   file = here("ensembles", "qra_model_type_adj_ensemble.csv"))

