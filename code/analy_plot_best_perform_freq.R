library(here)
library(masterthesis)
library(ggplot2)
library(scoringutils)
library(patchwork)
library(dplyr)
library(data.table)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))

avails <- hub_data |>
  select(model, availability, location, target_type) |>
  distinct()

sloc_mods <- hub_data |>
  select(model, location) |>
  distinct() |>
  group_by(model) |>
  summarise(count = n()) |>
  filter(count == 1) |>
  left_join(avails, by = "model") |>
  select(model, location, target_type) |>
  mutate(sloc = 1)

num_mods <- hub_data |>
  select(model, location, target_type, forecast_date) |>
  distinct() |>
  filter(forecast_date >= "2021-04-12",
         !grepl("EuroCOVID", model)) |>
  group_by(location, target_type, forecast_date) |>
  summarise(num_mods = n())

bpset <- fread(here("ensembles", "weights", "best_performers_incl_mods.csv")) |>
  mutate(incl = ifelse(location %in% c("PL", "DE") & nmod == 10 | location %in% c("CZ", "FR", "GB") & nmod == 5, 1, 0)) |>
  filter(incl == 1) |>
  left_join(num_mods, by = c("location", "target_type", "forecast_date")) |>
  filter(nmod < num_mods) |> #exclude instances where no "choosing" happened
  mutate(fct = num_mods/nmod) |>
  group_by(model, location, target_type) |>
  summarise(count = sum(fct)/2) |>
  right_join(avails, by = c("model", "location", "target_type")) |>
  filter(availability > 0.15) |>
  mutate(count = count / availability) |>
  left_join(sloc_mods, by = c("model", "location", "target_type")) |>
  mutate(sloc = as.numeric(!is.na(sloc)))
  

#cols <- brewer.pal(n=2,"Set2")

plot1 <- ggplot(bpset, aes(x = count)) +
  geom_histogram(aes(y=..density..),binwidth = 5, color="#1f4d3e", fill="#66C2A5", breaks = seq(0,45, by = 5)) +
  theme_masterthesis() +
  ylab("frequency") +
  xlab("") +
  facet_wrap(~target_type,
             labeller = as_labeller(specs$plot_target_label)) +
  ylim(0, 0.085)

plot2 <- ggplot(bpset |> filter(sloc == 1), aes(x = count)) +
  geom_histogram(aes(y=..density..),binwidth = 5, color="#2d3d62", fill="#8DA0CB", breaks = seq(0,45, by = 5)) +
  theme_masterthesis() +
  ylab("frequency") +
  xlab("") +
  facet_wrap(~target_type,
             labeller = as_labeller(specs$plot_target_label)) +
  xlim(0, 45) +
  ylim(0, 0.085)


overall_plot <- 
  (plot1) /
  (plot2) +
  plot_layout(guides = "collect", 
              heights = c(1, 1)) &
  plot_annotation(tag_levels = 'I') 


pdf(here("plots", "bestperform_freq.pdf"), width = 10, height = 5)
overall_plot
dev.off()
