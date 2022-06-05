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
#' @param may_miss number of forecasts within the rolling window a model
#'             may miss to be eligible for the best performers ensemble
#' @param excl models to exclude from the ensemble
#' @param cvg_threshold minimum coverage required for models to be 
#'            included in the sampling process
#'          

best_performers_ensemble <- function(data,
                                     score = "interval_score",
                                     nmod = 3,
                                     window = 4,
                                     may_miss = 0, 
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
  
  locs <- unique(data$location)
  targets <- unique(data$target_type)
  
  
  stop_ind <- length(dates) - window #not window-1 since we still need one obs for forecasting
  
  #result containers
  bestperf_ens <- NULL
  
  
  #fill a list with a matrix for each combination of location and target_type
  # column of each matrix are the models, rows are the forecast dates
  list_names <- c("all", paste(rep(locs, each = length(targets)), 
                               targets, sep = "."))
  
  model_matrices <- vector(mode = "list", 
                           length = length(list_names))
  names(model_matrices) <- list_names
  
  #each list element is the same initial matrix
  model_matrices <- lapply(model_matrices, function(x) 
    matrix(ncol = length(unique(data$model)),
           nrow = stop_ind,
           dimnames = list(as.character(dates[window+1:stop_ind]),
                           unique(data$model)
           )))
  
    
  for(i in 1:stop_ind){
    
    #dates that are in current sliding window
    sub_dates <- seq.Date(from = dates[i],
                          to = dates[i] + (window - 1) * 7,
                          by = 7)
    
    #get models that are available at given forecast date
    fc_date <- dates[i] + window * 7
    avail_models <- data |>
      filter(forecast_date == as.Date(fc_date)) |>
      select(model, target_type, location) |>
      distinct() |>
      mutate(is_present = 1)
      
    
    #find best performers in subset
    best_models <- data |>
      filter(forecast_date %in% sub_dates) |>
      merge(avail_models, #kick out models which don't predict at given forecast_date
            by = c("model", "location", "target_type")) |>
      group_by(model, target_type, location) |>
      mutate(n = n() / (23*4))  |> #forecast present => 92 rows, divide to get unique forecast presence 
      filter(n >= window - may_miss) |> #threshold to include model
      ungroup() |>
      score() |>
      summarise_scores(by = c("model", "location", "target_type")) |>
      group_by(location, target_type) |>
      filter(get(score) %in% sort(get(score))[1:nmod]) |>
      select(model, location, target_type)
    
    
    #just a small function that matches unique models in best_models
    #to column names in model matrices, to fill a row of the model matrix
    match_models <- function(data, model_matrix){
      mod_names <- unique(data$model)
      matches <- match(colnames(model_matrix), mod_names)
      return(as.numeric(!is.na(matches)))
    }
    
    
    #fill list of model matrices with above function
    for(comb in names(model_matrices)){
      if(comb == "all"){
        model_matrices[["all"]][i,] <- match_models(best_models, 
                                                    model_matrices[["all"]])
      } else {
        #split up combination of location and target
        filters <- strsplit(comb, split = "[.]")[[1]]
        
        #only keep those best models that match location and target
        comb_models <- best_models |>
          filter(location == filters[1],
                 target_type == filters[2])
        
        model_matrices[[comb]][i,] <- match_models(comb_models,
                                                   model_matrices[[comb]])
        
      }
    }

    
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
    return(list(bestperf_ens, model_matrices))
  } else {
    return(bestperf_ens)
  }
  
}