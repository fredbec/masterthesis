library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

plot_model_availability(hub_data)
plot_trajectories(hub_data)