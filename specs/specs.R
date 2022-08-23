dates <- c("2021-03-15","2022-01-31")

avail_threshold <- 0.35

locs <- c("DE", "PL", "GB", "FR", "CZ")

excl_neg <- TRUE

su_cols <- c("model", "forecast_date", "quantile", "horizon", 
             "target_type", "location", "target_end_date", 
             "prediction", "true_value")


#########make categorization of periods########
fc_dates <- sort(unique(hub_data$forecast_date))
fc_dates <- c(fc_dates, fc_dates[length(fc_dates)] + 7)
splitter <- c(1,10,10,9,9,9) |> cumsum()

#list for plots (overlapping dates to avoid gaps)
period_cat_plots <- lapply(1:5, function(i) 
  c(fc_dates[splitter[i]], 
    fc_dates[(splitter[i+1])]))

#list with endpoints as correct
period_cat <- lapply(period_cat_plots,
                     function(dat) c(dat[1], dat[2]-7))

#data.table for joining with hub_date
period_cat_dt <- lapply(seq_along(period_cat), 
       function(k) data.table(forecast_date = 
                                seq.Date(period_cat[[k]][1], 
                                         period_cat[[k]][2], by = 7),
                              period_cat = k)) |>
  rbindlist() |>
  mutate(period_cat = factor(period_cat))




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
all_combs_ensemble_small_nmod <- seq(3,10, by = 1)

#large set 
all_combs_ensemble_big_countries <- c("PL", "DE")
all_combs_ensemble_big_nmod <- c(seq(3,10, by = 1), 12, 14)


#plots
plot_horizon_label <- c(`1` = "1 week ahead", `2` = "2 weeks ahead",
                  `3` = "3 weeks ahead",`4` = "4 weeks ahead")
plot_target_label <- c(`Cases` = "Target: Cases", `Deaths` = "Target: Deaths")
plot_location_label <- c(`PL` = "Poland", `DE` = "Germany",
                         `CZ` = "Czech Rep.", `GB` = "Great Br.",
                         `FR` = "France")


specs <- list(dates = dates,
              avail_threshold = avail_threshold,
              locs = locs,
              excl_neg = excl_neg,
              su_cols = su_cols,
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
              all_combs_ensemble_big_nmod = all_combs_ensemble_big_nmod,
              plot_horizon_label = plot_horizon_label,
              plot_target_label = plot_target_label,
              plot_location_label = plot_location_label,
              period_cat_plots = period_cat_plots,
              period_cat = period_cat,
              period_cat_dt = period_cat_dt)

rm(dates, avail_threshold, locs, excl_neg, su_cols, 
   model_similarity_kickout_avail_threshold, 
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
   all_combs_ensemble_big_nmod, all_combs_ensemble_big_countries,
   plot_horizon_label, plot_target_label, plot_location_label,
   period_cat_plots, period_cat, period_cat_dt)