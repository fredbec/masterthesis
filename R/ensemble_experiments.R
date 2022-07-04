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
      scoringutils::summarise_scores(
        by = c("model", "location", "target_type")) |>
      dplyr::group_by(location, target_type) |>
      dplyr::slice_min(order_by = get(score), 
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
#' @param seed random seed for sampling
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



#only for one location now
all_combs_ensemble <- function(data, 
                               nmod = 3, 
                               window = 5,
                               init_weeks = 5,
                               avail_threshold = 0.5,
                               avail_overlap_threshold = 0.1,
                               excl = c("EuroCOVIDhub-baseline",
                                        "EuroCOVIDhub-ensemble")){
  data <- data |>
    filter(!model %in% excl,
           availability >= avail_threshold)
  
  fc_dates <- data |>
    select(forecast_date) |>
    distinct() |>
    pull() |>
    sort()
  
  result_dat <- NULL
  
  for(i in (init_weeks+1):length(fc_dates)){

    fc_date <- as.Date(fc_dates[i])
    
    subdat <- data |>
      filter(forecast_date == fc_dates[i])

    avail_models <- unique(subdat$model)
 
    
    ######forecast_date <= or <
    hist_dist_all <- data |>
      filter(model %in% avail_models,
             forecast_date < as.Date(fc_date)) |>
      model_dist(avail_threshold,
                 avail_overlap_threshold,
                 cramers_dist)
    hist_dist_all <- hist_dist_all[[1]]
    
    
    #recent history dates (according to window)
    recent_dates <- seq.Date(
      fc_date - (window * 7), #not window +1
      fc_date - 7,
      by = 7
    )
    
    #saving some time at init 
    if(i == (init_weeks+1)){
      recent_dist_all <- hist_dist_all
    } else {
      ######forecast_date <= or <
      recent_dist_all <- data |>
        filter(model %in% avail_models,
               forecast_date %in% recent_dates) |>
        model_dist(avail_threshold, 
                   avail_overlap_threshold, 
                   cramers_dist)
      
      recent_dist_all <- recent_dist_all[[1]]
    }
    
    #update avail_models (sometimes a model is available at fc_date,
    #but not in the history before)
    ############ALSO NEED TO DO THIS SEP. FOR RECENT#########
    avail_models <- rownames(hist_dist_all)
    
    #make all possible combinations of size nmod
    all_combs <- combn(avail_models, nmod) |> t()
    
    for(j in 1:nrow(all_combs)){
      ens_comb <- all_combs[j,]
      
      ens_dat <- subdat |>
        filter(model %in% ens_comb)
      
      #make all pairs from ens_comb
      ens_combs <- combn(ens_comb, 2) |> t()
      
      #compute historical distance metrics
      hist_dist_mat <- apply(ens_combs, 1, 
                             function(x) hist_dist_all[x[1], x[2]])
      mean_hist_dist <- mean(hist_dist_mat, na.rm = TRUE)
      sd_hist_dist <- sd(hist_dist_mat, na.rm = TRUE)

      
      #compute recent distance metrics
      recent_dist_mat <- apply(ens_combs, 1, 
                               function(x) recent_dist_all[x[1], x[2]])
      mean_recent_dist <- mean(recent_dist_mat, na.rm = TRUE)
      sd_recent_dist <- sd(recent_dist_mat, na.rm = TRUE)

      
      ens_dat <- ens_dat |>
        make_ensemble(mean) |>
        make_ensemble(extra_excl = "mean_ensemble") |>
        filter(model %in% c("mean_ensemble",
                            "median_ensemble")) |>
        mutate(inc_models = paste(ens_comb, collapse = ";"),
               nmod = nmod,
               mean_hist_dist = mean_hist_dist, 
               sd_hist_dist = sd_hist_dist,
               mean_recent_dist = mean_recent_dist,
               sd_recent_dist = sd_recent_dist)
      
      result_dat <- result_dat |>
        rbind(ens_dat)
      

    }
  }
  return(result_dat)
}