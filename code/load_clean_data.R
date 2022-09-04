library(data.table)
library(dplyr)
library(here)
here::i_am("code/load_clean_data.R")

#read in specs
source(here("specs", "specs.R"))

#read in filter values for locations and dates
avail_threshold <- specs$load_data_avail_threshold
forecast_dates <- specs$load_data_forecast_dates
locs <- specs$load_data_locs
excl_neg <- specs$load_data_excl_neg


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


#interpolate dates for filtering
dates <- forecast_dates |>
  as.Date() |>
  (\(x) seq.Date(x[1], x[2], by = 7))()

###Filter data according to specs
##and merge with model type info
hub_data <- left_join(hub_data, model_types,
                      by = "model") |>
  filter(location %in% locs,
         forecast_date %in% dates)

if(excl_neg){
  hub_data <- hub_data |>
    filter(true_value >= 0) |>
    mutate(prediction = ifelse(prediction < 0, 0, prediction))
}

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

#join with main data
hub_data <- hub_data |> 
  left_join(matcher, by = c("model", "target_end_date", "horizon")) |>
  mutate(act_1220 = as.numeric(!is.na(act_1220)))

#subset only those observations that need to be changed
hub_data_bl20 <- hub_data  |>
  filter(act_1220 == 1) |>
  mutate(forecast_date = as.IDate("2021-12-20")) |>
  select(-act_1220)

#attach again to orginal data
hub_data <- hub_data |>
  filter(act_1220 == 0) |> #remove relevant obs
  select(-act_1220) |>
  rbind(hub_data_bl20)|> #add back relevant obs
  setkey(model) |> #key is removed in the process
  arrange(model, location, target_type, forecast_date, 
          horizon, quantile) #|>


#compute availability and remove models that are under threshold
hub_data <- hub_data |>
  model_availability() |>
  filter(availability > avail_threshold) 

#remove some stuff from the workspace
rm(list = c("model_types", "dates", "locs", "matcher", "hub_data_bl20", "excl_neg", "forecast_dates"))