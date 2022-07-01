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

plot_best_performers_scores(best_performers_scores)


best_performers_models <- readRDS(here("results", "best_performers_chosen_models.RDS"))

plot_best_performers_models(best_performers_models, hub_data)
