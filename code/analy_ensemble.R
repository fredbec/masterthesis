library(scoringutils)
library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_make_ensemble.R"))

here::i_am("code/analy_ensemble.R")

cvg_threshold <- scan(here("specs", "cvg_threshold.txt"), numeric())


#make mean and median ensemble and score
hub_data <- hub_data |>
  make_ensemble(summary_function = mean) |>
  make_ensemble(summary_function = median, 
                extra_excl = c("mean_ensemble")) 


score(hub_data)

