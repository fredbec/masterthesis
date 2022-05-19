library(ggplot2)
library(here)
library(viridis)


#load hub_data
source(here("code", "load_clean_data.R"))
source(here("code", "fun_model_coverage.R"))


here::i_am("code/plot_trajectories.R")

locs <- readRDS(here("specs", "locs.RDS"))

hub_data <- hub_data |>
  filter(location %in% locs)


plot_model_avail_total <- function(data){
  data |>
    group_by(forecast_date, location, target_type) |>
    summarise(nmods = length(unique(model))) |>
    ungroup() |>
    ggplot(aes(x = forecast_date, y = nmods, 
               group = location, 
               color = location)) +
    geom_line() + 
    labs(title = "Total Model Availability") +
    facet_grid(~ target_type) +
    ylim(5, 25)
}

plot_model_avail_indiv <- function(data){
  data |>
    model_coverage() |>
    select(model, location, 
           target_type, coverage) |>
    distinct() |>
    ggplot(aes(x = location, y = model, 
               fill = coverage)) +
    geom_tile()+
    facet_wrap(~target_type) + 
    scale_fill_viridis(option = "rocket") +
    theme(axis.text.y = 
            element_text(angle = 0, size = 7)) +
    labs(title = "Individual Model Availability")
}

pdf(here("plots", "model_coverage.pdf"))
plot_model_avail_total(hub_data)
plot_model_avail_indiv(hub_data)
dev.off()