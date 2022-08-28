devtools::load_all()
library(masterthesis)
library(scoringutils)
library(dplyr)
library(here)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))

locs <- c("DE", "PL")

#nmods <- c(3,4)
#nmocs <- c(5,6,7)
nmods <- c(8, 12)
prop_nmod <- c(4, 4)
names(prop_nmod)
#prop_nmod <- list(6,6,5,5,4,4)
#names(prop_nmod) <- nmods

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
      if(nmod >= 7 & subset_fc_dates[[1]][i]=="2021-07-05" & loc == "DE"){
        all_combs_dat_cases <- readRDS(here("results", "all_combs_ensemble", 
                                      paste0("nmod", nmod, "_", loc,"_set", i, "_Cases.RDS")))
        
        add_model_dat <- add_model_to_ens(all_combs_dat = all_combs_dat_cases, 
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
        
        saveRDS(add_model_dat[[1]], here("results", "add_model", paste0("ensembles_nmod", nmod, "_", loc,"_set", i, "_Cases.RDS")))
        saveRDS(add_model_dat[[2]], here("results", "add_model", paste0("scores_nmod", nmod, "_", loc,"_set", i, "_Cases.RDS")))
        
        rm(add_model_dat)
        
        all_combs_dat_deaths <- readRDS(here("results", "all_combs_ensemble", 
                                            paste0("nmod", nmod, "_", loc,"_set", i, "_Deaths.RDS")))
        add_model_dat <- add_model_to_ens(all_combs_dat = all_combs_dat_deaths, 
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
        
        saveRDS(add_model_dat[[1]], here("results", "add_model", paste0("ensembles_nmod", nmod, "_", loc,"_set", i, "_Deaths.RDS")))
        saveRDS(add_model_dat[[2]], here("results", "add_model", paste0("scores_nmod", nmod, "_", loc,"_set", i, "_Deaths.RDS")))
        rm(add_model_dat)
        
        
        
      } else {
        all_combs_dat <- readRDS(here("results", "all_combs_ensemble", 
                                      paste0("nmod", nmod, "_", loc,"_set", i, ".RDS")))
        
        if(nrow(all_combs_dat) > 12000000){
          message("got here")
          all_combs_dat <- all_combs_dat |>
            slice_sample(prop = 0.5)
        }
        
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
        rm(add_model_dat)
      }
      
      
      
      ####
      print(unique(all_combs_dat$forecast_date))
      
      
    }
  }
}