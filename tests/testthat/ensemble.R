library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

here::i_am("tests/testthat/test_fun_make_ensemble.R")

#compare results using only germany and ensemble using all countries
#and filtering germany after --> should get same result
post_vsn <- make_ensemble(hub_data, 
                          summary_function = mean) |>
  filter(location == "DE")

pre_vsn <- hub_data |>
  filter(location == "DE") |>
  make_ensemble(summary_function = mean)

expect_true(all.equal(post_vsn, pre_vsn,
                      ignore.row.order = TRUE))


#same as above, but with dates
dates_check <- seq.Date(as.Date("2021-03-22"), 
                        as.Date("2021-05-17"), by = 7)

post_vsn <- make_ensemble(hub_data,
                          summary_function = mean) |>
  filter(forecast_date %in% dates_check)

pre_vsn <- hub_data |>
  filter(forecast_date %in% dates_check) |>
  make_ensemble(summary_function = mean)

expect_true(all.equal(post_vsn, pre_vsn,
                      ignore.row.order = TRUE))


#check if we get a warning if we don't exclude ensembles 
excl_check <- c("EuroCOVIDhub-baseline")

expect_warning(make_ensemble(hub_data, summary_function = mean, 
                             excl = excl_check),
               "(\"EuroCOVIDhub-ensemble\")")


############test if mean/median values are computed correctly##############

#choose random values from list of models and forecast_dates (thus over time
#more values will be tested)
set.seed(52)
loc <- unique(hub_data$location)|>
  sample(1)

models <- hub_data |>
  filter(location == loc, 
         !(model %in% c("EuroCOVIDhub-baseline", 
                        "EuroCOVIDhub-ensemble")),
         availability > 0.5) |>
  (\(x) unique(x$model))() |>
  sample(4)

date <- unique(hub_data$forecast_date) |>
  sample(1)
quan <- unique(hub_data$quantile) |>
  sample(1)
target <- unique(hub_data$target_type) |>
  sample(1)
hor <- unique(hub_data$horizon) |>
  sample(1)

#"manual" calculation
vals <- hub_data |>
  filter(location == loc,
         model %in% models,
         forecast_date == as.Date(date),
         quantile == quan,
         target_type == target,
         horizon == hor)

pred_mean <- mean(vals$prediction)
pred_median <- median(vals$prediction)

#calculation using make_ensemble 
ensemble_check <- make_ensemble(hub_data, mean, incl = models)|>
  make_ensemble(median, incl = models)

preds_check <- ensemble_check |>
  filter(location == loc,
         model %in% c("median_ensemble", 
                      "mean_ensemble"),
         forecast_date == as.Date(date),
         quantile == quan,
         target_type == target,
         horizon == hor)

expect_equal(pred_mean, 
             preds_check[preds_check$model == "mean_ensemble", "prediction"])
expect_equal(pred_median, 
             preds_check[preds_check$model == "median_ensemble", "prediction"])



#check that we get a single value (this has been an issue before and I
#am not 100% sure that I fixed it)
ensemble_check <- make_ensemble(hub_data, mean)

preds_check <- ensemble_check |>
  filter(location == loc,
         model == "mean_ensemble",
         forecast_date == as.Date(date),
         quantile == quan,
         target_type == target,
         horizon == hor)

expect_equal(nrow(preds_check), 1)