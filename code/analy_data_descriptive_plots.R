library(here)
library(masterthesis)
library(scoringutils)
library(gridExtra)

source(here("code", "load_clean_data.R"))

plot_model_availability(hub_data)
plot_trajectories(hub_data)



#####Rae Scores####
myplots <- list()
for(loc in unique(hub_data$location)){
  
  myplots[[loc]] <- hub_data |>
    filter(availability >0.5) |>
    filter(location == loc) |>
    score() |>
    summarise_scores(by = c("model", "target_type")) |>
    summarise_scores(fun = signif, digits = 2) |>
    plot_score_table(y = "model", by = "target_type") +
    facet_wrap( ~ target_type) +
    ggtitle(loc)
  
}

pdf(here("plots", "raw_scores.pdf"), 
    width = 9, height = 12)
for(loc in unique(hub_data$location)){
  print(myplots[[loc]])
}
dev.off()


###plot distributional forecasts
hub_data |>
  score() |>
  summarise_scores(by = c("model", 
                          "location", 
                          "target_type"))

filter(model == "LANL-GrowthRate") |>
  ggplot(aes(x = interval_score)) +
  geom_histogram() + 
  facet_wrap(target_type ~ location, scales = "free",
             nrow = 2, ncol = 5) +
  ggtitle("LANL")


hub_data |>
  score() |>
  filter(model == "ITWW-county_repro") |>
  ggplot(aes(x = interval_score)) +
  geom_histogram() + 
  facet_wrap(target_type ~ location, scales = "free",
             nrow = 2, ncol = 5) +
  ggtitle("ITWW")

