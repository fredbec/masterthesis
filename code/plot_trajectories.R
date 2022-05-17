rm(list = ls())

here::i_am("code/plot_trajectories.R")

library(dplyr)
library(ggplot2)

#load hub_data
source(here("code", "load_clean_data.R"))


pdf("plots/trajectories.pdf",
    height = 8.3, 
    width = 14.7)
hub_data |>
  filter(location %in% c("DE", "PL", "GB", "FR", "CZ"),
         quantile == 0.5,
         horizon == 1) |>
  mutate(incidence = 100000 * (true_value / population)) |> #for easier comparison
  select(location, target_end_date, true_value, 
         target_type, incidence) |>
  distinct() |>
  ggplot(aes(x = target_end_date, y = incidence, 
             group = location, color = location)) +
  geom_line(size = 1.2) + 
  scale_color_brewer(palette = "GnBu") + 
  facet_wrap(~target_type,
             scales = "free") #plots need different scales
dev.off()

  