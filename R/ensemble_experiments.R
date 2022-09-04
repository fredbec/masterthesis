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
                                     su_cols,
                                     score = "interval_score",
                                     nmods = 3,
                                     window = 4,
                                     may_miss = 1, 
                                     #excl = c("EuroCOVIDhub-baseline",
                                    #          "EuroCOVIDhub-ensemble"),
                                     excl = c("EuroCOVIDhub-ensemble"),
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
  
  ########################### CHECK THIS##################
  ###could just be length(dates)################
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
    print(sub_dates)
    #get models that are available at given forecast date
    fc_date <- dates[i] + window * 7
    print(fc_date)
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
      dplyr::filter(target_end_date < fc_date) |>
      dplyr::filter(n >= window - may_miss) |> #threshold to include model
      dplyr::ungroup() |>
      dplyr::select(su_cols) |>
      scoringutils::score() |>
      scoringutils::pairwise_comparison(
        by = c("model", "location", "target_type"), 
        baseline = "EuroCOVIDhub-baseline") |>
      dplyr::filter(compare_against == "EuroCOVIDhub-baseline",
                    model != "EuroCOVIDhub-baseline") |>
      dplyr::group_by(location, target_type) |>
      dplyr::slice_min(order_by = scaled_rel_skill, #only keep best performers
                       n = nmods) |>
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
  
  #for each comb only keep columns of models that actually forecast that comb
  for(comb in names(model_matrices)){
    filters <- strsplit(comb, split = "[.]")[[1]]

    present_models <- data |>
      dplyr::filter(location == filters[1],
                    target_type == filters[2]) |>
      select(model) |>
      distinct() |>
      pull()
    model_matrices[[comb]] <- 
      model_matrices[[comb]][, present_models]
  }
  
  ##score ensemble
  score_best_ens <- bestperf_ens |>
    scoringutils::score() |>
    scoringutils::summarise_scores(
      by = c("model", "target_type",
             "location", "horizon")) |>
    dplyr::mutate(best_perf = 1)
  
  
  normal_ens <- hub_data |>
    make_ensemble(mean) |>
    make_ensemble(extra_excl = c("mean_ensemble")) |>
    filter(model %in% c("mean_ensemble", "median_ensemble")) |>
    scoringutils::score() |>
    scoringutils::summarise_scores(
      by = c("model", "target_type",
             "location", "horizon")) |>
    dplyr::mutate(best_perf = 0)
  
  score_best_ens <- score_best_ens |>
    rbind(normal_ens)
  
  
  if(return_model_data){
    return(list(bestperf_ens, model_matrices, score_best_ens))
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
                              nmods = c(5, 6, 7, 8),
                              samples = 100,
                              seed = 32,
                              excl = c("EuroCOVIDhub-baseline",
                                       "EuroCOVIDhub-ensemble")){
  
  set.seed(seed)
  
  #make model list per country (getmodels is a function in utils)
  per_loc <- split(data,
                   by = c("location", "target_type"))
  models_per_loc <- lapply(
    per_loc,
    function(locdat) getmodels(locdat,
                               excl = excl,
                               avail_threshold = avail_threshold)
    )
  
  
  #get all targets, locations
  target_types <- unique(data$target_type)
  locs <- unique(data$location)
  locs_list <- lapply(locs, function(x) c(x))  #only individual locations for now
  
  #append "all" to nmods (to compute reference scores with all models)
  nmods <- c(lapply(nmods, function(x) c(x)), "all")
  
  #container for overall results
  result_table <- NULL
  
  
  for (loc in locs_list){
    for (target in target_types){
      for (nmod in nmods){
        
        score_tabs <- list()
        for (i in 1:samples){
          
          if (nmod == "all"){
            models <- models_per_loc[[paste0(loc, ".", target)]]

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
        
        #bind to all previous results
        result_table <- rbind(result_table, temp)
        
      }
    }
  }
  
  return(result_table)
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' @import scoringutils
#' 
#' @description 
#' Assesses ensemble performance when leaving out models that are too similar
#' 
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param avail_threshold mini
#' @param avail_overlap_thresh minimum overlap availability for pair of models 
#'          to be considered in distance calculation
#' @param max_nmods maximum number of models that should be kicked out (all numbers
#'          lower as this will also be iterated through)
#' @param model_dists optional; precalculated distance matrix (will be computed
#'          if none is passed)
#' @param dist_fun which distance function to use
#' @param excl which models should be excluded from the ensemble experiment

#' @export
#' 

model_similarity_kickout <- function(data, 
                                     avail_threshold,
                                     avail_overlap_threshold,
                                     max_nmods,
                                     model_dists = NULL,
                                     dist_fun = cramers_dist,
                                     excl = c("EuroCOVIDhub-baseline",
                                              "EuroCOVIDhub-ensemble")){
  
  data <- data |>
    filter(!model %in% excl,
           availability >= avail_threshold)
  
  targets <- unique(data$target_type)
  locs <- unique(data$location)
  
  
  if(is.null(model_dists)){
    model_dists <- model_dist(data, 
                              avail_threshold, 
                              avail_overlap_threshold,
                              dist_fun)
  }
  
  #result container
  result_table <- NULL
  
  for (loc in locs){
    for (target in targets){
      
      #access model dist matrix
      dist_matrix <- model_dists[[paste0(loc, ".", target)]]

      subdat <- data |>
        dplyr::filter(location == loc,
                      target_type == target)
      mod_kick <- NULL
      for (nmod in 0:max_nmods){
        
        if(nmod > 0){
          
          #get pair of models with minimum distance
          model_inds <- which(
            dist_matrix == min(dist_matrix, na.rm = TRUE),
            arr.ind = TRUE)[,1]
        
          #get sum of distance models have with others
          mod1_overall_dist <- sum(dist_matrix[model_inds[1],], na.rm = TRUE)
          mod2_overall_dist <- sum(dist_matrix[model_inds[2],], na.rm = TRUE)
          
          mod1 <- rownames(dist_matrix)[model_inds[1]]
          mod2 <- colnames(dist_matrix)[model_inds[2]]
          
          if(mod1_overall_dist < mod2_overall_dist){
            mod_kick <- c(mod_kick, mod1)
            mod_kick_ind <- model_inds[1]
          } else {
            mod_kick <- c(mod_kick, mod2)
            mod_kick_ind <- model_inds[2]
            
          }
          

          #eliminate corresponding row and column
          dist_matrix[mod_kick_ind,] <- NA
          dist_matrix[,mod_kick_ind] <- NA
          

        }
        
        
        result_table <- make_ensemble(subdat, 
                                      extra_excl = mod_kick) |>
          make_ensemble(summary_function = mean,
                        extra_excl = c(mod_kick, "median_ensemble")) |>
          dplyr::mutate(nmod = nmod,
                        mod_kick = paste(mod_kick,
                                         collapse = ", ")) |>
          dplyr::filter(model %in% c("median_ensemble",
                                     "mean_ensemble")) |>
          scoringutils::score() |>
          scoringutils::summarise_scores(by = c("model", "target_type", 
                                                "location", "nmod",
                                                "horizon")) |>
          rbind(result_table)
        

        
        #backdrop of random model kickout to gauge how effective kicking out model based on sim. is
        #two version: by pairwise elimination vs. sum of distances
        #analyze disagreement btw models kicked out for cases, deaths
        
        #judge by sum of other distances
        
      }
    }
  }
  return(result_table)
  
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
#' @param seed random seed mfor sampling
#' @param excl which models should be excluded from the ensemble experiment

#' @export


kickout_ensemble <- function(data,
                             avail_threshold, 
                             nmods,
                             samples,
                             seed,
                             excl = c("EuroCOVIDhub-baseline",
                                      "EuroCOVIDhub-ensemble")){
  
  set.seed(seed)
  
  data <- data |>
    dplyr::filter(!model %in% excl,
                  availability >= avail_threshold)
  
  
  #make model list per country (getmodels is a function in utils)
  per_loc <- split(data,
                   by = c("location", "target_type"))
  
  
  #get all targets, locations
  target_types <- unique(data$target_type)
  locs <- unique(data$location)
  locs_list <- lapply(locs, function(x) c(x))  #only individual locations for now
  

  #container for overall results
  result_table <- NULL
  
  
  for (nmod in nmods){
    
    score_tabs <- vector("list", samples)
    
    for(i in 1:samples){
      
      
      #sample nmod models to randomly kick from ensemble
      model_kick <- data |> 
        dplyr::select(model, target_type, 
                      location, forecast_date) |>
        dplyr::distinct() |>
        dplyr::group_by(forecast_date, 
                        target_type, 
                        location) |>
        dplyr::slice_sample(n = nmod) |>
        #identifier column for models to be kicked out after merging
        dplyr::mutate(kickout = 1) |>
        dplyr::arrange(forecast_date, location, target_type)
      
      
      #merge with original data to obtain set of models for ensemble, 
      #then make ensemble
      ensembles <- data |>
        dplyr::left_join(model_kick, 
                         by = c("model", "target_type",
                                "location", "forecast_date")) |>
        #only keep those instances of models-target-loc combination
        #that are not in model kick
        dplyr::filter(is.na(kickout)) |>
        dplyr::select(-kickout) |>
        make_ensemble(summary_function = median) |>
        make_ensemble(summary_function = mean, 
                      extra_excl = "median_ensemble") |>
        dplyr::filter(model %in% c("mean_ensemble",
                                   "median_ensemble")) |>
        dplyr::mutate(nmod = nmod)
      
      #score current ensembles
      score_tabs[[i]] <- ensembles |>
        scoringutils::score() |>
        scoringutils::summarise_scores(
          by = c("model", "target_type",
                 "location", "nmod",
                 "horizon")) |>
        mutate(sample_id = i)
      
    }
    
    #average over score tabs and bind to data.table with unaveraged
    #individual samples
    temp <- data.table::rbindlist(score_tabs) |>
      dplyr::group_by(model, target_type, 
                      location, nmod,
                      horizon) |>
      dplyr::summarise_all(mean) |>
      dplyr::ungroup() |>
      data.table::as.data.table() |>
      mutate(sample_id = "avg") |>
      #bind to table with individual samples
      rbind(data.table::rbindlist(score_tabs))
    
    
    #bind to all previous results
    result_table <- rbind(result_table, temp)
  }
  
  return(result_table)
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import scoringutils
#' @import tidyr
#' @description 
#' Computes inverse score weights for model in European Forecast Hub data
#' Also includes option to exponentially smooth weights
#'
#' @param score_data score table output from leaveout_ensemble function
#' @param score which score to evaluate (needs to match column names as 
#'              induced by scoringutils' score() function)
#'              default is interval_score
#' @param givedata should relative score data be returned instead of plot
#' @param title optional, alternative title
#' @param saveplot should plot be saved in pdf format?
#' @param path where to save plot to 
#' 
#' @export



inverse_score_weights <- function(data,
                                  score_data = NULL,
                                  su_cols = NULL,
                                  fc_date,
                                  at_level = "model",
                                  window = 4,
                                  exp_smooth = NULL,
                                  by_target_end_date = TRUE,
                                  score_fun = "interval_score",
                                  may_miss = 1){
  
  #####IMPUTE SCORES#######
  
  #warning if baseline or ensemble in data
  if(score_fun != "interval_score"){
    stop("can only impute scores for the WIS")
  }
  
  
  if(is.null(score_data) & is.null(su_cols)){
    stop("if not supplying scores, must supply su_cols")
  } else if (is.null(score_data)){
    score_data <- data |>
      select(su_cols) |>
      score() |>
      summarise_scores(by = c("model", "location", "target_type",
                              "forecast_date", "horizon"))
  }
  
  if(!setequal(unique(data$model), unique(score_data$model))){
    stop("data and score_data do not contain the same models")
  }
  
  #map of forecast_date and horizon to target_end_date
  tg_end_map <- data |> 
    select(forecast_date, horizon, target_end_date) |>
    distinct()
  
  
  #get window of dates
  curr_dates <- fc_dates_window(fc_date, window, incl = FALSE)
  
  
  #check if current dates in data and score data
  num_missing_data <- setdiff(curr_dates, unique(data$forecast_date)) |> length()
  num_missing_scores <- setdiff(curr_dates, unique(score_data$forecast_date)) |> length()
  if(num_missing_data > 0){
    message(paste(setdiff(curr_dates, unique(data$forecast_date)),
                  collapse = ", "))
    stop("dates needed to compute weights missing from data")
  }
  if(num_missing_scores> 0){
    message(paste(setdiff(curr_dates, unique(score_data$forecast_date)),
                  collapse = ", "))
    stop("dates needed to compute weights missing from score_data")
  }
  
  
  
  #compute score smoother values i
  if(!is.null(exp_smooth)){
    smoothing_vals <- tg_end_map |>
      #only keep dates of relevant forecasts
      filter(forecast_date <= fc_date,
             target_end_date < fc_date) |>
      #determine how many dates behind the forecast is
      mutate(lag_behind = ((as.IDate(fc_date) - 
                              target_end_date) + 5) / 7) |>
      mutate(smoother = exp_smooth * 
               (1 - exp_smooth)^(lag_behind - 1))
  } else { 
    #equal weights for all past observations
    smoothing_vals <- tg_end_map |>
      #only keep dates of relevant forecasts
      filter(forecast_date <= fc_date,
             target_end_date < fc_date) |>
      mutate(smoother = 1)
  }
  
  #append weights to data
  #(actually, now comes at a later point)
  #score_data <- score_data |>
  #  left_join(smoothing_vals, 
  #            by = c("forecast_date", "target_end_date", "horizon"))
  
  
  
  #make full sets (where all models predict for all targets in window)
  #to determine number of missings and impute missing scores
  full_sets <- score_data |>
    filter(forecast_date %in% curr_dates) |>
    split(by = c("target_type", "location")) |>
    lapply(function(dat)
      tidyr::crossing(placeholder = unique(dat[,at_level]), #rename afterwards because character
                      location = unique(dat$location),
                      target_type = unique(dat$target_type),
                      forecast_date = unique(dat$forecast_date),
                      horizon = 1:4)) |>
    rbindlist() |>
    left_join(tg_end_map, by = c("forecast_date", "horizon"))

  names(full_sets)[1] <- at_level
  
  #compute ivnerse score weights
  inv_score_weights <- score_data |>
    #get data from window
    #keep in unresolved horizon forecasts for counting (remove after)
    filter(forecast_date %in% curr_dates) |>
    mutate(present = 1) |> #this is to determine missings
    full_join(full_sets, 
              by = c(at_level, "location", "target_type", 
                     "forecast_date", "horizon", "target_end_date"))  |>
    mutate(present = ifelse(is.na(present), 0, 1)) |>
    #count number of unique forecasts for each model
    group_by(get(at_level), location, target_type) |>
    mutate(count = sum(present)/4) |> #divide by 4 because of horizon
    ungroup() |>
    filter(count >= (window - may_miss),
           target_end_date < fc_date) |> #remove as yet unresolved horizons 
    #impute missing scores (stratify by location, target_type, horizon)
    group_by(across(all_of(c("location", "target_type", "horizon")))) |>
    mutate(maxscore = max(get(score_fun), na.rm = TRUE)) |>
    filter(maxscore >= 0) |>
    ungroup() |>
    mutate(interval_score = ifelse(is.na(interval_score),  #actual imputation
                                   maxscore, interval_score)) |>
    select(-c(present, maxscore)) |>
    #join with exponential smoothing values
    left_join(smoothing_vals, 
              by = c("forecast_date", "target_end_date", "horizon")) |>
    #calculate inverse scores
    group_by(across(all_of(c(at_level, "location", "target_type")))) |>
    summarise(interval_score = weighted.mean(get(score_fun),
                                             w = smoother), 
              .groups = "drop") |>
    select(all_of(c(at_level, "target_type", "location", score_fun))) |>
    mutate(inv_score = 1/get(score_fun)) |>
    #normalize so sum of weights is 1 
    group_by(target_type, location) |> 
    mutate(weights = inv_score / sum(inv_score)) |>
    select(all_of(c(at_level, "location", "target_type", "weights"))) |>
    #add back forecast date
    mutate(forecast_date = fc_date)
  
  return(inv_score_weights)
  
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import quantgen
#' @import purrr
#' @description 
#' Computes QRA weights for data from European Forecast Hub
#'
#' @param data Data to form ensemble
#' @param curr_dates

qr_weights_one_inst <- function(data,
                                fc_date,
                                window,
                                taus){
  
  if(length(unique(data$location))>1 | length(unique(data$target_type))>1){
    stop("function only supports computation for one location at a time")
  }
  
  #compute current dates
  curr_dates <- fc_dates_window(fc_date, window)
  
  #convert data into wide format
  wide_data <- data |> 
    dplyr::filter(forecast_date %in% curr_dates) |>
    #dplyr::group_by(model, location, target_type) |>
    #dplyr::mutate(count = n()/92) |> #divide by 4 for horizon 
    #dplyr::filter(count >= length(curr_dates),
    #              target_end_date < max(curr_dates) + 7) |> #remove as yet unresolved horizons
    #prepare for dcast
    dplyr::mutate(quantile = paste0("quantile_", quantile)) |>
    data.table::setDT() |>
    data.table::dcast(model + target_type + location + forecast_date + 
                        horizon + true_value + target_end_date ~ quantile, 
                      value.var = "prediction") |>
    dplyr::arrange(horizon, location, model, forecast_date) |>
    dplyr::select(-c(forecast_date, target_end_date, target_type, location)) |>
    setkey(NULL)
  
  models <- unique(wide_data$model)
  
  
  true_values <- wide_data |>
    dplyr::group_by(model) |>
    dplyr::mutate(n = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(n) |>
    dplyr::summarise(true_value = unique(true_value), .groups = "drop") |>
    dplyr::select(true_value) |>
    dplyr::pull()
  
  
  
  if(!(length(true_values) == (nrow(wide_data))  / length(models))){
    stop("true values and models do not align")
  }
  #return(wide_data)
  qarr <- wide_data |>
    dplyr::select(-c(true_value, horizon)) |>
    split(by = "model", keep.by = FALSE) |>
    purrr::map(.f = as.matrix) |>
    quantgen::combine_into_array()
  
  
  model_weights <- quantgen::quantile_ensemble(qarr = qarr,
                                               y = true_values,
                                               lp_solver = "gurobi",
                                               tau = taus)$alpha |>
    data.table() |>
    dplyr::rename(weights = V1) |>
    dplyr::mutate(model = models,
                  forecast_date = fc_date) 
  
  
  
  return(model_weights)
}


qra_weights <- function(data,
                        fc_date,
                        window = 4){
  
  #####IMPUTE SCORES#######
  if(!as.Date(fc_date) %in% unique(data$forecast_date)){
    stop("fc_date not in data")
  }
  
  #map of forecast_date and horizon to target_end_date
  tg_end_map <- data |> 
    select(forecast_date, horizon, target_end_date) |>
    distinct()
  
  taus <- unique(data$quantile)
  
  #get window of dates
  curr_dates <- fc_dates_window(fc_date, window, incl = FALSE)
  
  #
  if(fc_date <= (as.Date("2021-05-10") + (window-1)*7)){
    idx_remove <- data.table(
      model = rep("stat_ensemble",12),
      location = rep(c("CZ", "GB", "PL"), each = window),
      target_type = rep("Cases",12),
      forecast_date = rep(seq.Date(as.Date("2021-05-10"),
                                   as.Date("2021-05-10") + (window-1) * 7,
                                   by = 7), 3)) |>
      mutate(forecast_date = as.IDate(forecast_date))
  } else {
    idx_remove <- data.table(
      model = NA,
      location = NA,
      target_type = NA,
      forecast_date = NA)
  }

  all_data <- data |>
    dplyr::filter(forecast_date %in% curr_dates) |>
    #dplyr::group_by(model, location, target_type) |>
    #dplyr::mutate(count = n()/92) |> #divide by 4 for horizon 
    dplyr::filter(#count == window,
                  target_end_date < fc_date) |> #remove as yet unresolved horizons
    setDT() |>
    anti_join(idx_remove, by = c("model", "location", "target_type", "forecast_date")) |>
    split(by = c("location", "target_type"))

  all_weights <- lapply(all_data, function(dat)
    qr_weights_one_inst(dat, fc_date, window, taus)) |>
    rbindlist(idcol = TRUE) |>
    tidyr::separate(.id, into = c("location", "target_type"), sep = "[.]") |>
    dplyr::arrange(model, location, target_type, forecast_date, weights)
    
  
  return(all_weights)
  
  
  
}
