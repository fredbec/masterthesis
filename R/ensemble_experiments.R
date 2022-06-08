#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' @import scoringutils
#' 
#' @description
#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param score which scoring function should be used to select best models
#' @param nmods number of models to sample
#' @param window number of weeks to consider in establishing what 
#'             the best models are
#' @param may_miss number of forecasts within the rolling window a model
#'             may miss to be eligible for the best performers ensemble
#' @param excl models to exclude from the ensemble
#' @param return_model_data if set to TRUE, additionally to the ensemble predictions,
#'         a list of matrices indicating which models were used in which step
#'         are returned
#' 
#' @returns by default, just the ensemble predictions. If `return_model_data`
#'        is set to TRUE, returns a list with the ensemble predictions as well
#'        as a list of matrices that indicate which model was used in each round 
#'        of the best performers ensemble
#'            
#' @export
#'          

best_performers_ensemble <- function(data,
                                     score = "interval_score",
                                     nmods = 3,
                                     window = 4,
                                     may_miss = 0, 
                                     excl = c("EuroCOVIDhub-baseline",
                                              "EuroCOVIDhub-ensemble"),
                                     return_model_data = FALSE){
  
  
  #filter out models that are in excl
  data <- data |>
    dplyr::filter(!model %in% excl)
  
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
      dplyr::filter(forecast_date == as.Date(fc_date)) |>
      dplyr::select(model, target_type, location) |>
      dplyr::distinct() |>
      dplyr::mutate(is_present = 1)
    
    
    #find best performers in subset
    best_models <- data |>
      dplyr::filter(forecast_date %in% sub_dates) |>
      merge(avail_models, #kick out models which don't predict at given forecast_date
            by = c("model", "location", "target_type")) |>
      dplyr::group_by(model, target_type, location) |>
      dplyr::mutate(n = n() / (23*4))  |> #forecast present => 92 rows, divide to get unique forecast presence 
      dplyr::filter(n >= window - may_miss) |> #threshold to include model
      dplyr::ungroup() |>
      scoringutils::score() |>
      scoringutils::summarise_scores(by = c("model", "location", "target_type")) |>
      dplyr::group_by(location, target_type) |>
      dplyr::filter(get(score) %in% sort(get(score))[1:nmods]) |>
      dplyr::select(model, location, target_type)
    
    
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
          dplyr::filter(location == filters[1],
                        target_type == filters[2])
        
        model_matrices[[comb]][i,] <- match_models(comb_models,
                                                   model_matrices[[comb]])
        
      }
    }
    
    
    #keep only those models which are in best_models and make 
    #next week's prediction out of these models
    ens_preds <- data |>
      dplyr::filter(forecast_date == (sub_dates[window] + 7)) |>
      merge(best_models, by = c("model", "location", 
                                "target_type")) |>
      make_ensemble(summary_function = mean) |>
      make_ensemble(summary_function = median, 
                    extra_excl = "mean_ensemble") |>
      dplyr::filter(model %in% c("mean_ensemble",
                          "median_ensemble"))
    
    bestperf_ens <- rbind(bestperf_ens, ens_preds)
    
  }
  
  if(return_model_data){
    return(list(bestperf_ens, model_matrices))
  } else {
    return(bestperf_ens)
  }
  
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' @import scoringutils
#' 
#' @description 
#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param avail_threshold minimum availability for models to be considered in the
#'            sampling process 
#' @param nmods number of models that should be sampled for each step
#' @param samples how many random samples of models to draw 
#' @param seed random seed for sampling
#' @param excl which models should be excluded from the ensemble experiment

#' @export

leaveout_ensemble <- function(data,
                              avail_threshold,
                              nmods = c(5, 6, 7, 8, 100),
                              samples = 100,
                              seed = 32,
                              excl = c("EuroCOVIDhub-baseline",
                                       "EuroCOVIDhub-ensemble")){
  
  set.seed(seed)
  
  #to obtain models that are above avail threshold and not in excl
  getmodels <- function(data){
    
    models <- data |>
      dplyr::filter(availability >= avail_threshold) |>
      (\(x) unique(x$model))() |>
      (\(x) x[-which(x %in% excl)])()
    
    #check if there are still ensembles
    is_ensemble <- grepl(".*ensemble.*", models)
    if(any(is_ensemble)){
      warning(paste0("There seems to be at least one ensemble (\"" , 
                     paste(models[is_ensemble], collapse = "\", \""), 
                     "\") that is not in the list of excluded models. Is this intended?"))
    }
    
    return(models)
  }
  
  #make model list per country
  per_loc <- split(data,
                   by = c("location", "target_type"))
  models_per_loc <- lapply(per_loc, FUN = getmodels)
  
  target_types <- unique(data$target_type)
  locs <- unique(data$location)
  #locs_list <- c(list(locs), lapply(locs, function(x) c(x)))
  #############***#########
  #for now: only individual locations:
  locs_list <- lapply(locs, function(x) c(x))
  
  #container for overall results
  result_table <- NULL
  
  for (loc in locs_list){
    for (target in target_types){
      for (nmod in nmods){
        
        score_tabs <- list()
        for (i in 1:samples){
          if (nmod == 100 | nmod == "all"){
            models <- models_per_loc[[paste0(loc, ".", target)]]
            nmod <- "all"
          } else {
            models <- sample(models_per_loc[[paste0(loc, ".", target)]], 
                             nmod)
          }
          
          #entry for location column
          loc_col <- ifelse(length(loc)>1, "all", loc)
          
          #make ensembles with current set of models
          ensembles <- data |>
            dplyr::filter(location %in% loc,
                          model %in% models,
                          target_type == target) |>
            make_ensemble(summary_function = mean) |>
            make_ensemble(summary_function = median,
                          extra_excl = c("mean_ensemble")) |>
            dplyr::filter(model %in% c("median_ensemble",
                                       "mean_ensemble")) |>
            dplyr::mutate(nmod = nmod)
          #location = loc_col,
          #target_type = target,
          #)
          
          #score resulting ensembles
          score_tabs[[i]] <- ensembles |>
            scoringutils::score() |>
            scoringutils::summarise_scores(by = c("model", "target_type", 
                                                  "location", "nmod")) 
          
          #no need for multiple samples with all models (no randomness)
          if(nmod == "all"){break}
          
        }
        #average over all tables for set of nmod + loc 
        temp <- data.table::rbindlist(score_tabs) |>
          dplyr::group_by(model, target_type, location, nmod) |>
          dplyr::summarise_all(mean) |>
          dplyr::ungroup() |>
          data.table::as.data.table()
        
        result_table <- rbind(result_table, temp)
        
      }
      
    }
  }
  return(result_table)
  
}
