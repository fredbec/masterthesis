rm(list = ls())

setwd("C:/Users/rike/Nextcloud/Uni/Master/5. Semester (WiSe2122)/Nikos_MA/master_thesis")

library(data.table)
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
                               filter_both = list("model == 'epiforecasts-EpiNow2'", "location == 'DE'"),
                               filter_forecasts = list("forecast_date == '2021-04-05'"), 
                               filter_truth = list("as.Date(target_end_date) <= '2021-05-03'"),
                               x = "target_end_date", 
                               range = c(0, 50, 90), 
                               scale = "free",
                               facet_formula = target_type ~ model)

#plot_locs_per_model(hub_data)
#plot_models_per_loc(hub_data)



hub_sub <- hub_data |>
  filter(target_type == "Deaths",
         horizon == 2,
         model_type == "ensemble") |>
  mutate(range = abs(quantile-0.5)*200)


scores <- scoringutils::eval_forecasts(hub_sub, 
                                       summarise_by = c("model", "range", "quantile", "horizon"))

scoringutils::quantile_coverage(scores) + 
  ggplot2::ggtitle("Interval Coverage")

scores <- scoringutils::eval_forecasts(hub_sub, 
                                       summarise_by = c("model"))


scoringutils::score_table(scores)
print(scores)




hub_sub <- hub_data |>
  filter(model_type == "mechanistic") |>
  mutate(range = abs(quantile-0.5)*200)

scores <- scoringutils::eval_forecasts(hub_sub, 
                                       summarise_by = c("model", "range","target_type"))
scoringutils::range_plot(scores, y = "interval_score", 
                         facet_formula = ~ target_type)


scores <- scoringutils::eval_forecasts(hub_sub, 
                                       summarise_by = c("model", "horizon"))
scores <- scores[, horizon := as.factor(horizon)]
scoringutils::score_heatmap(scores, 
                            x = "horizon", metric = "bias")
