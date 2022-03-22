rm(list = ls())

setwd("C:/Users/rike/Nextcloud/Uni/Master/5. Semester (WiSe2122)/Nikos_MA/master_thesis")

library("data.table")
library(magrittr)
library(ggplot2)
library(dplyr)

source("code/load_clean_data.R")


plot_models_per_loc <- function(data) {
  data |>
    group_by(location, forecast_date) |>
    mutate(n = length(unique(model))) |>
    ggplot(aes(y = reorder(location, n), x = as.Date(forecast_date), fill = n)) +
    geom_tile() +
    facet_wrap(~ target_type) +
    labs(y = "Location", x = "Forecast date")
}


plot_locs_per_model <- function(data) {
  data |>
    group_by(model, forecast_date) |>
    mutate(n= length(unique(location))) |>
    ggplot(aes(y = reorder(model, n), x = as.Date(forecast_date), fill = n)) +
    geom_tile() +
    facet_wrap(~ target_type) +
    labs(y = "Location", x = "Forecast date")
}


scoringutils::plot_predictions(data = hub_data,
                               filter_both = list("location == 'DE'"),
                               filter_forecasts = list("forecast_date == '2021-06-07'"), 
                               filter_truth = list("as.Date(target_end_date) <= '2021-06-12'"),
                               x = "target_end_date", 
                               range = c(0, 50, 90), 
                               scale = "free",
                               facet_formula = target_type ~ model)

#plot_locs_per_model(hub_data)
#plot_models_per_loc(hub_data)



hub_sub <- hub_data |>
  filter(target_type == "Deaths",
         horizon == 2)


scores <- scoringutils::eval_forecasts(hub_sub, 
                                       summarise_by = c("model", "quantile", "horizon"))

print(scores)