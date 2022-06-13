library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))
here::i_am("tests/testthat/test-wasserstein.R")



#make artificial dataframe where distance is predefined
#and check whether the function actually computes the correct distance 
qs <- seq(0.25, 0.75, by = 0.05)
models <- paste0("model", 1:3)
dates <- seq.Date(as.Date("2021-05-03"), 
                  as.Date("2021-05-10"), 
                  by = 7)
horizon <- 1:2

n_rows <- length(qs)*length(models)*length(dates)*length(horizon)

mydat <- expand.grid(forecast_date = dates, 
                     model = models, 
                     quantile = qs, 
                     horizon = horizon) |>
  arrange(forecast_date, horizon, model, quantile) |>
  mutate(prediction = runif(n_rows),
         availability = 1,
         location = "myloc",
         target_type = "mytarget")


#predefined distance (mod2 is always 1 away from mod1, mod3 is always 0.25 away from mod1)
mylen <- length(mydat[mydat$model == "model2", "prediction"])
mydat[mydat$model == "model2", "prediction"] <- mydat[mydat$model == "model1", "prediction"] + sample(c(-1, 1), mylen, replace = TRUE)
mydat[mydat$model == "model3", "prediction"] <- mydat[mydat$model == "model1", "prediction"] + 0.25

distmat <- wasserstein_dist(mydat, avail_threshold = 0)[[1]]

expect_equal(distmat[1,3], length(qs)*0.25^2)
expect_equal(distmat[1,2], length(qs))