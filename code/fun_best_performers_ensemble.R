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
#' @param excl models to exclude from the ensemble
#' @param cvg_threshold minimum coverage required for models to be 
#'            included in the sampling process
#'          

best_performers_ensemble <- function(data,
                                     score = "interval_score",
                                     nmod = 3,
                                     window = 4,
                                     excl = c("EuroCOVIDhub-baseline",
                                              "EuroCOVIDhub-ensemble"),
                                     cvg_threshold = NULL,
                                     return_model_data = FALSE){
  
  
  #filter out models that are in excl
  data <- data |>
    filter(!model %in% excl)
  
  dates <- data$forecast_date |>
    unique() |>
    as.Date() |>
    sort()
  
  
  stop_ind <- length(dates) - window #not window-1 since we still need one obs for forecasting
  
  #result containers
  bestperf_ens <- NULL
  model_data <- matrix(ncol = length(unique(data$model)), 
                       nrow = stop_ind)
  colnames(model_data) <- unique(data$model)
  rownames(model_data) <- as.character(dates[window+1:stop_ind])
    
  for(i in 1:stop_ind){
    
    #dates that are in current sliding window
    sub_dates <- seq.Date(from = dates[i],
                          to = dates[i] + (window - 1) * 7,
                          by = 7)
    
    #find best performers in subset
    best_models <- data |>
      filter(forecast_date %in% sub_dates) |>
      score() |>
      summarise_scores(by = c("model", "location", "target_type")) |>
      group_by(location, target_type) |>
      filter(get(score) %in% sort(get(score))[1:nmod]) |>
      select(model, location, target_type)
    
    
    match_models <- function(data, model_matrix){
      mod_names <- unique(data$model)
      matches <- match(colnames(model_matrix), mod_names)
      return(as.numeric(!is.na(matches)))
    }
    
    #mod_names <- unique(best_models$model)
    # matches <- match(colnames(model_data), mod_names)
    model_data[i,] <- match_models(best_models, model_data)#as.numeric(!is.na(matches))
    
    
    #keep only those models which are in best_models and make 
    #next week's prediction out of these models
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

  }

  if(return_model_data){
    return(list(bestperf_ens, model_data))
  } else {
    return(bestperf_ens)
  }
  
}