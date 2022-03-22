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
model_types <- read.csv("scraper/model_types.csv")
model_types <- model_types |>
  rename(model = model_abbr)


hub_data <- merge(hub_data, model_types, by = "model")


#write.csv(hub_data, "hub_data/hub_data.csv")
