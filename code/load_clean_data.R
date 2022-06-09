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
model_types <- read.csv(here("scraper", "metadata_extended.csv")) |>
  select(model_abbr, model_type)|>
  rename(model = model_abbr) |>
  mutate(model = gsub("\n| ","", model))


#read in filter values for locations and dates
locs <- specs$locs

dates <- specs$dates |>
  as.Date() |>
  (\(x) seq.Date(x[1], x[2], by = 7))()


hub_data <- merge(hub_data, model_types, 
                  by = "model") |>
  filter(location %in% locs,
         forecast_date %in% dates) |>
  model_availability() 


#remove some stuff from the workspace
rm(list = c("model_types", "dates", "locs"))