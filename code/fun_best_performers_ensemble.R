source(here("code", "fun_make_ensemble.R"))

#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#' Uses fun_make_ensemble.R
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param score which scoring function should be used to select best models
#' @param nmods number of models to sample
#' @param window number of weeks to consider in establishing what 
#'             the best models are
#' @param samples how many random samples of models to draw 
#' @param excl models to exclude from the ensemble
#' @param cvg_threshold minimum coverage required for models to be 
#'            included in the sampling process
#'          

best_performers_ensemble <- function(data,
                                     score = "interval_score",
                                     nmod = 3,
                                     window = 4,
                                     samples = 100,
                                     excl = c("EuroCOVIDhub-baseline",
                                              "EuroCOVIDhub-ensemble"),
                                     cvg_threshold = NULL){
  
  #start <- data |>
  #  select(forecast_date) |>
  #  distinct() |>
  #  mutate(forecast_date = sort(forecast_date)) |>
  #  slice_head(n = 1) |>
  #  pull(forecast_date) |>
  #  as.Date()
  
  #filter out models that are in excl
  data <- data |>
    filter(!model %in% excl)
  
  dates <- data$forecast_date |>
    unique() |>
    as.Date() |>
    sort()
  
  bestperf_ens <- NULL
  
  stop_ind <- length(dates) - window #not window-1 since we still need one obs for forecasting
  for(i in 1:stop_ind){
    
    sub_dates <- seq.Date(from = dates[i],
                          to = dates[i] + (window - 1) * 7,
                          by = 7)
    
    #find best performers in subset
    best_models <- data |>
      filter(forecast_date %in% sub_dates) |>
      score() |>
      summarise_scores(by = c("model", "location", "target_type")) |>
      group_by(location, target_type) |>
      filter(interval_score %in% sort(interval_score)[1:nmod]) |>
      select(model, location, target_type)
    
    
    #keep only those models which are in best_models
    ens_preds <- data |>
      filter(forecast_date == (sub_dates[window] + 7)) |>
      merge(best_models, by = c("model", "location", 
                                "target_type")) |>
      make_ensemble(summary_function = mean) |>
      make_ensemble(summary_function = median, 
                    extra_excl = "mean_ensemble") |>
      filter(model %in% c("mean_ensemble",
                          "median_ensemble"))

    bestperf_ens <- rbind(bestperf_ens, ens_preds)
    
    #score_tabs[[i]] <- bestperf_ens |>
    #  score() |>
    #  summarise_scores(by = c("model", "location", "target_type")) |>
    #  mutate(nmod = nmod,
    #         window = window,
    #         forecast_date = ) 
    
    #return(score_tabs)
    
    

  }
  
  #end <- start + ((window - 1) * 7)
  
  
  
  #dates <- seq.Date(start, end, by = 7)

  return(bestperf_ens)
  
}