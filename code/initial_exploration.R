rm(list = ls())

here::i_am("code/initial_exploration.R")

library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)

source(here("code", "load_clean_data.R"))


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

hub_data %>%
  filter(location %in% c("DE", "UK", "FR", "NL", "IT")) %>%
  make_NA(what = "truth", 
          target_end_date >= "2021-06-21", 
          target_end_date < "2021-03-15"
  ) %>%
  make_NA(what = "forecast",
          model != 'epiforecasts-EpiNow2', 
          forecast_date != "2021-05-24"
  ) %>%
  plot_predictions(
    x = "target_end_date",
    by = c("target_type", "location")
  ) +
  facet_wrap(target_type ~ location, ncol = 4, scales = "free") 

hub_data %>%
  filter(target_end_date <= "2021-06-21") %>%
  avail_forecasts(by = c("model", "forecast_date", "target_type")) %>%
  plot_avail_forecasts() +
  facet_wrap(~ target_type)


hub_data %>%
  score() %>%
  summarise_scores(by = c("model", "target_type"),
                  relative_skill = TRUE, 
                  baseline = "EuroCOVIDhub-baseline") %>%
  summarise_scores(fun = signif, digits = 2) %>%
  plot_score_table(by = "target_type") + 
  facet_wrap(~ target_type, nrow = 1)

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
