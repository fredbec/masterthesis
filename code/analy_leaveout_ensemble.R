library(scoringutils)
library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_ensemble.R"))

here::i_am("code/analy_leaveout_ensemble.R")

country <- c("DE")