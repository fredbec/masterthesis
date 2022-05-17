library(scoringutils)
library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_ensemble.R"))

here::i_am("code/analy_ensemble.R")

#model list for practice
practice_models <- c("CovidMetrics-epiBATS", "itwm-dSEIR", 
                     "MUNI-ARIMA", "EuroCOVIDhub-ensemble", 
                     "Karlen-pypm", "HZI-AgeExtendedSEIR" )

#define dataset for practice
#sub_hub <- hub_data |>
#  filter(location == "DE", 
#         forecast_date == "2021-08-02", 
#         model %in% practice_models,
#         quantile %in% seq(0.4, 0.6, by = 0.05)) |>
#  select(model, target_type, true_value, quantile, 
#         prediction, horizon, location, forecast_date,
#         target_end_date, population, n, model_type)


ger_data <- hub_data |>
  filter(location == "DE")

#make mean and median ensemble and score
mydat_mean <- make_ensemble(ger_data, summary_function = mean)
mydat_med <- make_ensemble(ger_data, summary_function = median,
                           extra_excl = c("mean_ensemble"))


  
