library(scoringutils)
library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_best_performers_ensemble.R"))
mydat <- best_performers_ensemble(hub_data, return_model_data = TRUE)