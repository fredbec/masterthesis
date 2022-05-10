rm(list = ls())

setwd("C:/Users/rike/Nextcloud/Uni/Master/5. Semester (WiSe2122)/Nikos_MA/master_thesis")

source("code/load_clean_data.R")
source("code/fun_ensemble.R")


library(dplyr)
library(scoringutils)

#model list for practice
practice_models <- c("CovidMetrics-epiBATS", "itwm-dSEIR", 
                     "MUNI-ARIMA", "EuroCOVIDhub-ensemble", 
                     "Karlen-pypm", "HZI-AgeExtendedSEIR" )

#define dataset for practice
sub_hub <- hub_data |>
  filter(location == "DE", 
         forecast_date == "2021-08-02", 
         model %in% practice_models,
         quantile %in% seq(0.4, 0.6, by = 0.05)) |>
  select(model, target_type, true_value, quantile, 
         prediction, horizon, location, forecast_date,
         target_end_date, population, n, model_type)


#make mean and median ensemble and score
mydat_mean <- make_ensemble(hub_data, summary_function = mean)
mydat_med <- make_ensemble(mydat_mean, summary_function = median,
                           excl = c("EuroCOVIDhub-baseline"))


  
