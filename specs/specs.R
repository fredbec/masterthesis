dates <- c("2021-03-15","2022-01-31")

avail_threshold <- 0.35

locs <- c("DE", "PL", "GB", "FR", "CZ")

excl_neg <- TRUE



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



######## model distance / all_combs_ensemble #############
#to compute distance dataframe
model_distance_avail_threshold <- 0
model_distance_avail_overlap_threshold <- 0
model_distance_dist_fun <- cramers_dist


all_combs_ensemble_avail_threshold <- 0
all_combs_ensemble_window <- 5
all_combs_ensemble_init_weeks <- 5

##small set (countries with less models -> CZ, FR, GB)
all_combs_ensemble_small_countries <- c("CZ", "FR", "GB")
all_combs_ensemble_small_nmod <- c(3,4,5,6,7,8,9,10,11,12)

#large set 
all_combs_ensemble_big_countries <- c("PL", "DE")
all_combs_ensemble_big_nmod <- c(3,4,6,8,10,12,14,16)



specs <- list(dates = dates,
              avail_threshold = avail_threshold,
              locs = locs,
              excl_neg = excl_neg,
              model_similarity_kickout_avail_threshold = model_similarity_kickout_avail_threshold,
              model_similarity_kickout_avail_overlap_threshold = model_similarity_kickout_avail_overlap_threshold,
              model_similarity_kickout_dist_fun = model_similarity_kickout_dist_fun, 
              model_similarity_kickout_number_random_samples = model_similarity_kickout_number_random_samples,
              model_similarity_kickout_max_nmods = model_similarity_kickout_max_nmods,
              model_similarity_kickout_nmods = model_similarity_kickout_nmods,
              model_similarity_kickout_random_seed = model_similarity_kickout_random_seed,
              best_performers_ensemble_nmods = best_performers_ensemble_nmods,
              best_performers_ensemble_window = best_performers_ensemble_window, 
              best_performers_ensemble_may_miss = best_performers_ensemble_may_miss,
              model_distance_avail_threshold = model_distance_avail_threshold,
              model_distance_avail_overlap_threshold = model_distance_avail_overlap_threshold,
              model_distance_dist_fun = model_distance_dist_fun,
              all_combs_ensemble_avail_threshold = all_combs_ensemble_avail_threshold,
              all_combs_ensemble_window = all_combs_ensemble_window,
              all_combs_ensemble_init_weeks = all_combs_ensemble_init_weeks,
              all_combs_ensemble_small_countries = all_combs_ensemble_small_countries,
              all_combs_ensemble_small_nmod = all_combs_ensemble_small_nmod,
              all_combs_ensemble_big_countries = all_combs_ensemble_big_countries,
              all_combs_ensemble_big_nmod = all_combs_ensemble_big_nmod)

rm(dates, avail_threshold, locs, model_similarity_kickout_avail_threshold, 
   model_similarity_kickout_avail_overlap_threshold, 
   model_similarity_kickout_dist_fun, model_similarity_kickout_number_random_samples,
   model_similarity_kickout_max_nmods,
   model_similarity_kickout_nmods, model_similarity_kickout_random_seed,
   best_performers_ensemble_nmods, best_performers_ensemble_window,
   best_performers_ensemble_may_miss,
   model_distance_avail_threshold, model_distance_avail_overlap_threshold,
   model_distance_dist_fun,
   all_combs_ensemble_avail_threshold, all_combs_ensemble_init_weeks, 
   all_combs_ensemble_window,
   all_combs_ensemble_small_countries, all_combs_ensemble_small_nmod,
   all_combs_ensemble_big_nmod, all_combs_ensemble_big_countries)