dates <- c("2021-03-15","2022-01-31")

avail_threshold <- 0.35

locs <- c("DE", "PL", "GB", "FR", "CZ")



########## model_similarity_kickout#########
model_similarity_kickout_avail_threshold <- 0.5
model_similarity_kickout_avail_overlap_threshold <- 0.35
model_similarity_kickout_dist_fun <- cramers_dist
model_similarity_kickout_number_random_samples <- 100
model_similarity_kickout_max_nmods <- 4
model_similarity_kickout_nmods <- c(1,2,3,4)
model_similarity_kickout_random_seed <- 32



########## best_performers ensemble ##########
best_performers_ensemble_nmods <- 5
best_performers_ensemble_window <- 4
best_performers_ensemble_may_miss <- 1


specs <- list(dates = dates,
              avail_threshold = avail_threshold,
              locs = locs,
              model_similarity_kickout_avail_threshold = model_similarity_kickout_avail_threshold,
              model_similarity_kickout_avail_overlap_threshold = model_similarity_kickout_avail_overlap_threshold,
              model_similarity_kickout_dist_fun = model_similarity_kickout_dist_fun, 
              model_similarity_kickout_number_random_samples = model_similarity_kickout_number_random_samples,
              model_similarity_kickout_max_nmods = model_similarity_kickout_max_nmods,
              model_similarity_kickout_nmods = model_similarity_kickout_nmods,
              model_similarity_kickout_random_seed = model_similarity_kickout_random_seed,
              best_performers_ensemble_nmods = best_performers_ensemble_nmods,
              best_performers_ensemble_window = best_performers_ensemble_window, 
              best_performers_ensemble_may_miss = best_performers_ensemble_may_miss)

rm(dates, avail_threshold, locs, model_similarity_kickout_avail_threshold, 
   model_similarity_kickout_avail_overlap_threshold, 
   model_similarity_kickout_dist_fun, model_similarity_kickout_number_random_samples,
   model_similarity_kickout_max_nmods,
   model_similarity_kickout_nmods, model_similarity_kickout_random_seed,
   best_performers_ensemble_nmods, best_performers_ensemble_window,
   best_performers_ensemble_may_miss)