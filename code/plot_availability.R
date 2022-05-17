library(ggplot2)
library(here)

#load hub_data
source(here("code", "load_clean_data.R"))

here::i_am("code/plot_trajectories.R")

locs <- readRDS(here("specs", "locs.RDS"))

hub_data <- hub_data |>
  filter(location %in% locs)


plot_model_avail <- function(data){
  data |>
    group_by(forecast_date, location, target_type) |>
    summarise(nmods = length(unique(model))) |>
    ungroup() |>
    ggplot(aes(x = forecast_date, y = nmods, group = location, color = location)) +
    geom_line() + 
    facet_grid(~ target_type) +
    ylim(5, 25)
}

pdf(here("plots", "availability.pdf"))
plot_model_avail(hub_data)
dev.off()