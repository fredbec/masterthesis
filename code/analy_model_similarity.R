#make heatmap (use normalized model distances)

model_dists_norm <- readRDS(here("results", "model_similarity_kickout_model_dists_norm.RDS"))


plot_model_similarity(model_dists_norm)
