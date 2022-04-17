rm(list = ls())

setwd("C:/Users/rike/Nextcloud/Uni/Master/5. Semester (WiSe2122)/Nikos_MA/master_thesis")

library("data.table")
library(magrittr)
#library(ggplot2)
library(dplyr)

hub_data <- rbindlist(
  list(
    fread("hub_data/data/full-data-european-forecast-hub-1.csv"),
    fread("hub_data/data/full-data-european-forecast-hub-2.csv"),
    fread("hub_data/data/full-data-european-forecast-hub-3.csv")
  )
)

#load model_types and rename column for merging
model_types <- read.csv("scraper/metadata_extended.csv") |>
  select(model_abbr, model_type)|>
  rename(model = model_abbr) |>
  mutate(model = gsub("\n| ","", model))


hub_data <- merge(hub_data, model_types, by = "model")


#write.csv(hub_data, "hub_data/hub_data.csv")
