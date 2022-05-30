library(scoringutils)
library(here)
library(ggplot2)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_leaveout_ensemble.R"))

here::i_am("code/analy_leaveout_ensemble.R")


leaveout_data <- leaveout_ensemble(hub_data, samples = 20)

saveRDS(leaveout_data, here("results", "leaveout_ensemble.RDS"))

leaveout_data <- readRDS(here("results", "leaveout_ensemble.RDS"))
plot_leaveout(leaveout_data)

