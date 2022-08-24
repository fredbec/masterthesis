devtools::load_all()
library(masterthesis)
library(scoringutils)
library(dplyr)
library(here)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))

locs <- c("DE", "PL")

#nmods <- c(3,4)
nmods <- c(5,6,7,8,9,10)
prop_nmod <- list(6,6,5,5,4,4)
names(prop_nmod) <- nmods

#needs to start with "2021-04-19", otherwise the following won't work
#but could of course always change it 
subset_fc_dates <- list(seq.Date(from = as.Date("2021-04-19"),
                                 to = as.Date("2022-01-31"),
                                 by = 7),
                        seq.Date(from = as.Date("2021-04-19"),
                                 to = as.Date("2022-01-31"),
                                 by = 7)
)
#names(subset_fc_dates) <- nmods


map_fc_to_tg <- hub_data |>
  select(forecast_date, horizon, target_end_date)


for(nmod in nmods){
  for (loc in locs){
    print(loc)
    for(i in 1:length(subset_fc_dates[[1]])){
      
      all_scores <- hub_data |>
        select(specs$su_cols) |>
        dplyr::filter(location == loc) |>
        score() |>
        summarise_scores(by = c("model",
                                "forecast_date",
                                "target_type",
                                "horizon")) |> 
        left_join(map_fc_to_tg, by = c("forecast_date", "horizon"))
      
      #make trycatch here in case dataset doesn't exist
      all_combs_dat <- readRDS(here("results", "all_combs_ensemble", 
                                    paste0("nmod", nmod, "_", loc,"_set", i, ".RDS")))
      
      ####
      print(unique(all_combs_dat$forecast_date))
      
      add_model_dat <- add_model_to_ens(all_combs_dat = all_combs_dat, 
                                        hub_data = hub_data,
                                        loc =loc,
                                        nmod = nmod,
                                        all_scores = all_scores,
                                        su_cols = specs$su_cols,
                                        fc_dates = subset_fc_dates[[1]][i],
                                        window = 4,
                                        sample_nmod = 100,
                                        seed = 41,
                                        prop_nmod = prop_nmod[[as.character(nmod)]])
      
      saveRDS(add_model_dat[[1]], here("results", "add_model", paste0("ensembles_nmod", nmod, "_", loc,"_set", i, ".RDS")))
      saveRDS(add_model_dat[[2]], here("results", "add_model", paste0("scores_nmod", nmod, "_", loc,"_set", i, ".RDS")))
    }
  }
}