rm(list = ls())

setwd("C:/Users/rike/Nextcloud/Uni/Master/5. Semester (WiSe2122)/Nikos_MA/master_thesis")

library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)

source("code/load_clean_data.R")

locs <- unique(hub_data$location)

plot_locs_per_model <- function(data) {
  data |>
    group_by(model, forecast_date) |>
    mutate(n= length(unique(location))) |>
    ggplot(aes(y = reorder(model, n), x = as.Date(forecast_date), fill = n)) +
    geom_tile() +
    facet_wrap(~ target_type) +
    labs(y = "Location", x = "Forecast date")
}

pdf(file=paste0("plots/coverage_alllocs.pdf"))
for (loc in locs){
  onew <- hub_data |>
    filter(location == loc) |>
    group_by(model, forecast_date) |>
    ggplot(aes(y = reorder(model, n), x = as.Date(forecast_date), fill = model_type)) +
    geom_tile() +
    facet_wrap(~ target_type) +
    labs(y = "Location", x = "Forecast date", title = paste0("location = ", loc))
  print(onew)
}
dev.off()


onew <- hub_data |>
  filter(location == 'DE',
         between(as.Date(forecast_date), 
                  as.Date("2021-01-07"), 
                  as.Date("2021-07-21")))

plot_locs_per_model(onew)
