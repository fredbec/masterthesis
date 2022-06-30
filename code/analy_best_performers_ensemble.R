library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

nmods <- specs$best_performers_ensemble_nmods
window <- specs$best_performers_ensemble_window
may_miss <- specs$best_performers_ensemble_may_miss


best_performers_results <- 
  best_performers_ensemble(hub_data,
                           nmods = nmods,
                           window = window,
                           may_miss = may_miss,
                           return_model_data = TRUE)

saveRDS(best_performers_results[[1]],
        here("results", "best_performers_ensemble.RDS"))
saveRDS(best_performers_results[[2]],
        here("results", "best_performers_chosen_models.RDS"))
saveRDS(best_performers_results[[3]],
        here("results", "best_performers_scores.RDS"))
#leaveout bad model

best_performers_scores <- readRDS(here("results", 
                                       "best_performers_scores.RDS"))

best_performers_scores |>
  dplyr::mutate(best_perf = factor(best_perf,
                                   levels = c(0,1),
                                   labels = c("hub-ens",
                                              "best-perf-ens")),
                model = factor(model,
                               levels = c("mean_ensemble",
                                          "median_ensemble"),
                               labels = c("mean-ens",
                                          "median-ens"))) |>
  dplyr::rename(method = model,
                strategy = best_perf) |>
  ggplot2::ggplot(aes(x = horizon, y = interval_score)) +
  ggplot2::geom_line(aes(color = method,
                         linetype = strategy)) +
  ggplot2::facet_wrap(location ~ target_type,
                      nrow = 5, ncol = 2,
                      scales = "free")
