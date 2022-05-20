library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_make_ensemble.R"))

here::i_am("code/tests/test_fun_model_coverage.R")

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


#check if we get an error if we don't exclude ensembles 
excl_check <- c("EuroCOVIDhub-baseline")

expect_warning(make_ensemble(hub_data, summary_function = mean, 
                             excl = excl_check),
               "(\"EuroCOVIDhub-ensemble\")")

