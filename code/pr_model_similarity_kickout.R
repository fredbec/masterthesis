##run on 23 June
library(masterthesis)
library(here)

source(here("specs", "specs.R"))
#loads hub_data
source(here("code", "load_clean_data.R"))

avail_threshold <- specs$model_similarity_kickout_avail_threshold
avail_overlap_threshold <- specs$model_similarity_kickout_avail_overlap_threshold
dist_fun <- specs$model_similarity_kickout_dist_fun

model_dists <- model_dist(hub_data,
                          avail_threshold = avail_threshold,
                          avail_overlap_threshold = avail_overlap_threshold,
                          dist_fun = dist_fun)

model_dists_norm <- model_dist(hub_data,
                               avail_threshold = avail_threshold,
                               avail_overlap_threshold = avail_overlap_threshold,
                               dist_fun = dist_fun,
                               normalize = TRUE)

saveRDS(model_dists, here("results", "model_similarity_kickout_model_dists.RDS"))
saveRDS(model_dists_norm, here("results", "model_similarity_kickout_model_dists_norm.RDS"))

model_dists <- readRDS(here("results", "model_similarity_kickout_model_dists.RDS"))


kickout_results <- model_similarity_kickout(hub_data, 
                                            avail_threshold = avail_threshold,
                                            avail_overlap_threshold = avail_overlap_threshold,
                                            nmods = 3,
                                            model_dists = model_dists)

saveRDS(kickout_results, here("results", "model_similarity_kickout_results.RDS"))
