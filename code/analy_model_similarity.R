library(here)
library(masterthesis)
#make heatmap (use normalized model distances)

model_dists_norm <- readRDS(here("results", "model_similarity_kickout_model_dists_norm.RDS"))
model_kickout <- readRDS(here("results", "model_similarity_kickout_results.RDS"))
random_model_kickout <- readRDS(here("results", "model_similarity_random_kickout_results.RDS"))

#make heatmap
plot_model_similarity(model_dists_norm)

#make kickout result plots
plot_kickout_results(model_kickout,
                     random_model_kickout,
                     "interval_score")
