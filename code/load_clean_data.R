library(data.table)
library(dplyr)
library(here)

source(here("code", "fun_model_coverage.R"))

here::i_am("code/load_clean_data.R")

hub_data <- rbindlist(
  list(
    fread(here("hub_data", "data", "full-data-european-forecast-hub-1.csv")),
    fread(here("hub_data", "data", "full-data-european-forecast-hub-2.csv")),
    fread(here("hub_data", "data", "full-data-european-forecast-hub-3.csv"))
  )
)

#load model_types and rename column for merging
model_types <- read.csv(here("scraper", "metadata_extended.csv")) |>
  select(model_abbr, model_type)|>
  rename(model = model_abbr) |>
  mutate(model = gsub("\n| ","", model))


#read in filter values for locations and dates
locs <-  scan(here("specs", "locs.txt"), 
              character(), sep = ",")

dates <- scan(here("specs", "dates.txt"), 
              character(), sep = ",") |>
  as.Date() |>
  (\(x) seq.Date(x[1], x[2], by = 7))()


hub_data <- merge(hub_data, model_types, 
                  by = "model") |>
  filter(location %in% locs,
         forecast_date %in% dates) |>
  model_coverage() |>
  mutate(cvg_incl = as.numeric(coverage >= 0.5)) |>
  select(-coverage)


#write.csv(hub_data, "hub_data/hub_data.csv")
