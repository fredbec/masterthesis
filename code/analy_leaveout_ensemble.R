library(scoringutils)
library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_leaveout_ensemble.R"))

here::i_am("code/analy_leaveout_ensemble.R")


levaour <- leaveout_ensemble(hub_data, samples = 5)

saveRDS(levaour, here("results", "leaveout_ensemble.RDS"))