library(data.table)
library(dplyr)
library(here)


here::i_am("code/load_clean_data.R")

#read in specs
source(here("specs", "specs.R"))


hub_data <- rbindlist(
  list(
    fread(here("hub_data", "data", "full-data-european-forecast-hub-1.csv")),
    fread(here("hub_data", "data", "full-data-european-forecast-hub-2.csv")),
    fread(here("hub_data", "data", "full-data-european-forecast-hub-3.csv"))
  )
)

#load model_types and rename column for merging
model_types <- read.csv(here("scraper", "metadata_extended_final.csv")) |>
  select(model_abbr, model_type)|>
  rename(model = model_abbr) |>
  mutate(model = gsub("\n| ","", model))


#read in filter values for locations and dates
locs <- specs$locs

dates <- specs$dates |>
  as.Date() |>
  (\(x) seq.Date(x[1], x[2], by = 7))()


hub_data <- left_join(hub_data, model_types,
                      by = "model") |>
  filter(location %in% locs,
         forecast_date %in% dates)


#some baseline predictions are wrongly attributed to "2021-12-27"
#helper dataframe to identify those observations
matcher <- data.frame(model = "EuroCOVIDhub-baseline",
                      target_end_date = seq.Date(
                        as.Date("2021-12-25"),
                        as.Date("2022-01-15"),
                        by = 7),
                      horizon = 1:4,
                      act_1220 = 1) |>
  mutate(target_end_date = as.IDate(target_end_date))

hub_data <- hub_data |> 
  left_join(matcher, by = c("model", "target_end_date", "horizon")) |>
  mutate(act_1220 = as.numeric(!is.na(act_1220)))

hub_data_bl20 <- hub_data  |>
  filter(act_1220 == 1) |>
  mutate(forecast_date = as.IDate("2021-12-20")) |>
  select(-act_1220)

hub_data <- hub_data |>
  filter(act_1220 == 0) |>
  select(-act_1220) |>
  rbind(hub_data_bl20)|>
  setkey(model) |>
  arrange(model, location, target_type, forecast_date, horizon, quantile) |>
  model_availability() 


#change so row order is same (with arrange)

#remove some stuff from the workspace
rm(list = c("model_types", "dates", "locs"))