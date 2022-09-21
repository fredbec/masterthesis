library(masterthesis)
library(here)
library(spatstat.geom)
source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

devtools::load_all()

window <- specs$best_performers_ensemble_window
nmods <- specs$best_performers_ensemble_nmods

best_performers_data <- NULL
k <- 1
for(nmod in nmods){
  
  if(nmod == 8){
    
    #exclude loc-target combs if they don't have enough models
    #here: only include GB&CZ Deaths, as well as DE&PL both
    bp_data <- hub_data |>
      filter(location %in% c("DE", "PL", "GB", "CZ")) |>
      mutate(excl = ifelse(target_type == "Cases" & 
                             location %in% c("GB", "CZ"), 
                           1, 0)) |>
      filter(excl == 0) |>
      select(-excl)
    
    lwr <- 2
    upr <- 7
  }
  if(nmod == 10){
    #exclude loc-target combs if they don't have enough models
    #here: only include DE&PL Deaths and Cases
    bp_data <- hub_data |>
      filter(location %in% c("DE", "PL"))
    lwr <- 2
    upr <- 5
  } else {
    bp_data <- hub_data
    lwr <- 2
    upr <- 11
  }
  
  best_performers_ens <- best_performers_ensemble(bp_data, su_cols = specs$su_cols,
                                                  nmods = nmod,
                                                  #may_miss = 3,
                                                  return_model_data = TRUE)
  
  best_performers_ensemble_mean <- best_performers_ens[[1]] |> 
    filter(model == "mean_ensemble")
  
  best_performers_ensemble_median <- best_performers_ens[[1]] |> 
    filter(model == "median_ensemble")
  
  
  #best performers models are returned as matrices because two months ago me is stupid
  best_performers_data[[k]] <-lapply(best_performers_ens[[2]][lwr:upr], function(mat)
    mat |> 
    data.table(keep.rownames = TRUE) |> 
    melt(id.vars = "rn") |> 
    filter(value == 1) |>
    arrange(rn) |> 
    select(-value) |>
    rename(forecast_date = rn)) |>
    rbindlist(idcol = TRUE) |>
    mutate(nmod = nmod) |>
    tidyr::separate(.id, into = c("location", "target_type"), sep = "[.]") |>
    rename(model = variable)
  

  data.table::fwrite(best_performers_ensemble_mean, 
                     here("ensembles", paste0("best_performers_ensemble_mean_nmod", nmod,"_window", window, ".csv")))
  data.table::fwrite(best_performers_ensemble_median, 
                     here("ensembles", paste0("best_performers_ensemble_median_nmod", nmod,"_window", window, ".csv")))
  
  
  k <- k + 1
}

data.table::fwrite(rbindlist(best_performers_data), here("ensembles", "weights", "best_performers_incl_mods.csv"))




####################Calculate corresponding inverse score weights####################################
#inverse score weights 
best_performers <- fread(here("ensembles", "weights", "best_performers_incl_mods.csv"))
#score data
score_data <- fread(here("score_data", "score_all_models.csv"))

fc_dates <- sort(unique(hub_data$forecast_date))[-(1:window)] |> 
  as.list()


all_inv_score_weights <- vector(mode = "list", length = length(nmods))
k <- 1
for(nm in nmods){
  
  subset_bp <- best_performers |>
    filter(nmod == nm)
  
  #hub data with only best performers at each forecast date
  bp_data <- right_join(hub_data, subset_bp,
                       by = c("location", "target_type", "forecast_date", "model")) |>
    arrange(forecast_date, location, target_type, model)
  
  #bp_scores <- right_join(score_data, subset_bp,
  #                        by = c("location", "target_type", "forecast_date", "model")) |>
  #  arrange(forecast_date, location, target_type, model)
  
  
  #helper function that gets subset of data (both hub and hub score data) 
  #with only the best performers of forecast date
  #Different to above in the following way: includes all the same
  #models (best performers at fcdate), needed to score models
  get_correct_subset <- function(bp_data,
                                 all_data,
                                 fcdate){
    bp_models <- bp_data |>
      filter(forecast_date == fcdate) |>
      select(location, target_type, model) |>
      distinct()
    
    subset_bp <- all_data |>
      right_join(bp_models, by = c("location", "target_type", "model"))
    
    return(subset_bp)
  }
  
  
  invscore_weights <- lapply(fc_dates, function(fcdat)
    
    inverse_score_weights(data = get_correct_subset(bp_data, hub_data, fcdat), 
                          score_data = get_correct_subset(bp_data, score_data, fcdat),
                          su_cols = NULL, 
                          fc_date = fcdat, 
                          exp_smooth = NULL, 
                          window = window)) |>
    rbindlist() |>
    mutate(nmod = nm)
  
  all_inv_score_weights[[k]] <- invscore_weights
  
  
  ######make according ensembles (weighted mean and median)
  bp_weighted_median_ens <- bp_data |>
    select(-nmod) |>
    left_join(invscore_weights, by = c("model", "location", 
                                       "target_type", "forecast_date")) |>
    select(-nmod) |>
    make_ensemble(summary_function = weighted.median,
                  model_name = "weighted.median_ensemble") |>
    filter(model == "weighted.median_ensemble")
  
  bp_weighted_mean_ens <- bp_data |>
    select(-nmod) |>
    left_join(invscore_weights, by = c("model", "location", 
                                       "target_type", "forecast_date")) |>
    select(-nmod) |>
    make_ensemble(summary_function = weighted.mean,
                  model_name = "weighted.mean_ensemble") |>
    filter(model == "weighted.mean_ensemble")
  
  
  data.table::fwrite(bp_weighted_mean_ens, 
                     here("ensembles", paste0("best_performers_ensemble_invscore_mean_nmod", nm,".csv")))
  data.table::fwrite(bp_weighted_median_ens, 
                     here("ensembles", paste0("best_performers_ensemble_invscore_median_nmod", nm,".csv")))
  
  k <- k + 1
  
}

all_inv_score_weights <- rbindlist(all_inv_score_weights) 
data.table::fwrite(all_inv_score_weights, here("ensembles", "weights", "best_performers_invscore_weights.csv"))
