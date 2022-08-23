#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#'
#' Computes availability per model and location over entire observation time
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param alldat should full data be returned with extra column for 
#'              availability added (the default) or only a dataframe with
#'              the availability data (columns are models, location,
#'              target_type)
#' @return either a data.frame with only the models and their respective
#'        availability, or the original data.table including and extra column
#'        with the availability values of the respective models (the default)
#'
#' @export       
#'        


model_availability <- function(data, alldat = TRUE){
  
  models <- unique(data$model)
  dates <- unique(as.Date(data$forecast_date))
  targets <- unique(data$target_type)
  locs <- unique(data$location)
  
  #data frame of all possible instances (combinations of model, fc_date)
  all_combs <- expand.grid(model = models, 
                           forecast_date = dates,
                           target_type = targets,
                           location = locs)
  
  #instances that are actually in the data
  actual_combs <- data |>
    dplyr::select(model, forecast_date, target_type, location) |>
    dplyr::distinct() |>
    dplyr::mutate(is_present = 1)
  
  #merge two dataframes to compute availability
  availability <- merge(all_combs, actual_combs,
                        by = c("model", "forecast_date", "target_type", "location"),
                        all.x = TRUE) |>
    dplyr::mutate(is_present = as.numeric(!is.na(is_present)),
                  model = as.character(model)) |>
    dplyr::group_by(model, target_type, location) |>
    dplyr::summarise(availability = mean(is_present)) |>
    dplyr::ungroup()
  
  if(alldat){
    data <- merge(data, availability, 
                  by = c("model", "target_type", "location")) |>
      data.table::setkey(model)
    return(data)
    
  } else {
    return(availability)
  }
  
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#'
#' Helper function that returns models given excl and an availability threshold
#' Also checks whether there still are ensembles in the data (this is because
#' this function is typically used with an ensembling procedure, where ensemble
#' models are not wanted as input)
#' 
#' @param data data (subset or full) from the European Forecast hub
#' @param excl vector of models to be excluded
#' @param incl vector of models to be included
#' @param avail_threshold threshold of availability for model to be included
#' 
#' @return either a data.frame with only the models and their respective
#'        availability, or the original data.table including and extra column
#'        with the availability values of the respective models (the default)
#'
#' @export 


getmodels <- function(data, 
                      excl = c("EuroCOVIDhub-baseline",
                               "EuroCOVIDhub-ensemble"),
                      incl = NULL,
                      avail_threshold = NULL){
  
  if(is.null(avail_threshold)){
    avail_threshold <- 0
  }
  
  if(!is.null(incl)){
    models <- data |>
      dplyr::filter(availability >= avail_threshold,
                    model %in% incl) |>
      (\(x) unique(x$model))() 
    
  } else if (!is.null(excl)){
    models <- data |>
      dplyr::filter(availability >= avail_threshold,
                    !model %in% excl) |>
      (\(x) unique(x$model))() 
    
  } else {
    models <- data |>
      dplyr::filter(availability >= avail_threshold) |>
      (\(x) unique(x$model))() 
  }
  
  #check if there are still ensembles
  is_ensemble <- grepl(".*ensemble.*", models)
  if(any(is_ensemble)){
    warning(paste0("There seems to be at least one ensemble (\"" , 
                   paste(models[is_ensemble], collapse = "\", \""), 
                   "\") that is not in the list of excluded models. Is this intended?"))
  }
  
  return(models)
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#'
#' Helper function that computes a distance measure for each pair of models
#' in the European Forecast Hub (a forecast in the Hub is distributional
#' and is comprised of a discrete set of 23 forecast quantiles)
#' 
#' @param data data (subset or full) from the European Forecast hub
#' @param avail_threshold minimum availability for a model to be considered
#' @param excl models to exclude from the calculation
#' 
#' @return either a data.frame with only the models and their respective
#'        availability, or the original data.table including and extra column
#'        with the availability values of the respective models (the default)

#' @export

model_dist <- function(data, 
                       avail_threshold,
                       avail_overlap_threshold,
                       dist_fun, 
                       excl = c("EuroCOVIDhub-baseline",
                                "EuroCOVIDhub-ensemble"),
                       normalize = FALSE,
                       return_symmetric = TRUE
                       ){
   
  #remove models that are excluded or don't meet availability threshold
  data <- data |>
    filter(!model %in% excl,
           availability >= avail_threshold)
  
  locs <- unique(data$location)
  targets <- unique(data$target_type)
  models <- unique(data$model)
  
  n_obs <- data |>
    select(forecast_date, quantile, horizon) |>
    distinct() |>
    nrow()
  
  #fill a list with a matrix for each combination of location and target_type
  # column of each matrix are the models, rows are the forecast dates
  list_names <- paste(rep(locs, each = length(targets)),
                      targets, sep = ".")
  
  model_matrices <- vector(mode = "list", 
                           length = length(list_names))
  names(model_matrices) <- list_names
  
  
  
  #loop over each combination of target and location
  for (comb in names(model_matrices)){ 
    
    #split up combination of location and target
    filters <- strsplit(comb, split = "[.]")[[1]]
  
    #get relevant subset of data
    subdat <- data |>
      dplyr::filter(location == filters[1],
                    target_type == filters[2])
    
    #make all pairwise combinations of models
    subdat_models <- unique(subdat$model)
    model_combs <- combn(subdat_models, 2) |> t()
    
    
    #compute distance for all model_combs
    #result is a vector, transfer to matrix after
    vec_result <- apply(model_combs, 1, function(x) 
      compute_pair_dist(x, subdat, n_obs, 
                        avail_overlap_threshold, cramers_dist)
      )
    
    
    mat_result <- matrix(NA, 
      length(subdat_models),length(subdat_models),
      dimnames = list(subdat_models, subdat_models)
      )
    #fill lower and upper triangular
    mat_result[lower.tri(mat_result)] <- vec_result
    mat_result <- t(mat_result)
    mat_result[lower.tri(mat_result)] <- vec_result
    
    #assign to result list
    model_matrices[[comb]] <- mat_result
    
    if (normalize){
      maxval <- max(model_matrices[[comb]], na.rm = TRUE)
      model_matrices[[comb]] <- 
        model_matrices[[comb]] / maxval
      
    }
  
  }
  return(model_matrices)  
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#'
#' Helper function that computes a distance measure for each pair of models
#' in the European Forecast Hub (a forecast in the Hub is distributional
#' and is comprised of a discrete set of 23 forecast quantiles)
#' 
#' @param data data (subset or full) from the European Forecast hub
#' @param avail_threshold minimum availability for a model to be considered
#' @param avail_overlap_threshold minimum availability overlap for a model pair  
#' to have their distance computed
#' @param dist_fun which distance function to use
#' @param excl models to exclude from the calculation
#' 
#' @return either a data.frame with only the models and their respective
#'        availability, or the original data.table including and extra column
#'        with the availability values of the respective models (the default)

#' @export

model_dist_by_fc_date <- 
  function(data, 
           avail_threshold,
           avail_overlap_threshold,
           dist_fun, 
           excl = c("EuroCOVIDhub-baseline",
                    "EuroCOVIDhub-ensemble")){
    
    #remove models that are excluded or don't meet availability threshold
    data <- data |>
      filter(!model %in% excl,
             availability >= avail_threshold)
    
    locs <- unique(data$location)
    targets <- unique(data$target_type)
    models <- unique(data$model)
    
    n_obs <- data |>
      select(forecast_date, quantile, horizon) |>
      distinct() |>
      nrow()
    
    #fill a list with a matrix for each combination of location and target_type
    # column of each matrix are the models, rows are the forecast dates
    list_names <- paste(rep(locs, each = length(targets)),
                        targets, sep = ".")
    
    comb_distances <- vector(mode = "list", 
                             length = length(list_names))
    names(comb_distances) <- list_names
    
    
    
    #loop over each combination of target and location
    for (comb in names(comb_distances)){ 
      
      #split up combination of location and target
      filters <- strsplit(comb, split = "[.]")[[1]]
      
      #get relevant subset of data
      subdat <- data |>
        dplyr::filter(location == filters[1],
                      target_type == filters[2])
      
      #make all pairwise combinations of models
      subdat_models <- unique(subdat$model)
      model_combs <- combn(subdat_models, 2) |> t()
      
      #compute distance for all model_combs
      #result is a vector, transfer to matrix after
      list_result <- apply(model_combs, 1, function(x) 
        compute_pair_dist_by_fc_date(
          x, subdat, n_obs, 
          avail_overlap_threshold, cramers_dist)
      )
      #convert to data.table
      dt_result <- data.table::rbindlist(list_result) |>
        dplyr::mutate(location = filters[1],
                      target_type = filters[2])
        
    

      #assign to result list
      comb_distances[[comb]] <- dt_result

      
    }
    
    model_distances <- data.table::rbindlist(comb_distances) 
    return(model_distances)  

    
}


compute_pair_dist <- function(model_combs, 
                              subdat, 
                              n_obs, 
                              avail_overlap_threshold, 
                              dist_fun){
  #for easier readability in pipe
  #these are the column names for the 2 models' preds after reshape
  mod1 <- paste0("prediction.", model_combs[1])
  mod2 <- paste0("prediction.", model_combs[2])
  

  model_comb_dat <- subdat |>
    dplyr::filter(model %in% model_combs) |> #get two relevant models
    dplyr::select(forecast_date, quantile, horizon, 
                  model, prediction) |>
    #reshape such that each models' prediction are own column
    reshape(idvar = c("forecast_date", "quantile", "horizon"), 
            timevar = "model",
            direction = "wide") |>
    #check for overall overlap in model availability
    mutate(both_avail = as.numeric(!is.na(get(mod1))&
                                     !is.na(get(mod2))))
  
  
  avail_overlap <- model_comb_dat |>
    summarise(overlap = mean(both_avail) * (n() / n_obs)) |>
    pull()
  
  if(avail_overlap < avail_overlap_threshold){
    
    return(NA)
  } else {
    dist_val <- model_comb_dat |>
      filter(!is.na(get(mod1)),
             !is.na(get(mod2))) |>
      dplyr::group_by(forecast_date, horizon) |>
      #compute distance metric
      dplyr::summarise(dist = dist_fun(
        get(mod1), get(mod2), quantile
      )) |>
      dplyr::ungroup() |>
      #take average over all quantiles and forecast_dates
      dplyr::summarise(avg_dist = mean(dist)) |>
      dplyr::pull()
    
    return(dist_val)
  }
}



compute_pair_dist_by_fc_date <- 
  function(model_combs, 
            subdat, 
            n_obs, 
            avail_overlap_threshold, 
            dist_fun){
  #for easier readability in pipe
  #these are the column names for the 2 models' preds after reshape
  mod1 <- paste0("prediction.", model_combs[1])
  mod2 <- paste0("prediction.", model_combs[2])
  
  
  model_comb_dat <- subdat |>
    dplyr::filter(model %in% model_combs) |> #get two relevant models
    dplyr::select(forecast_date, quantile, horizon, 
                  model, prediction) |>
    #reshape such that each models' prediction are own column
    reshape(idvar = c("forecast_date", "quantile", "horizon"), 
            timevar = "model",
            direction = "wide") |>
    #check for overall overlap in model availability
    mutate(both_avail = as.numeric(!is.na(get(mod1))&
                                     !is.na(get(mod2)))) |>
    #otherwise things get a little jumbled, producing warnings in
    #distance computation
    arrange(forecast_date, horizon, quantile)
  
  
  avail_overlap <- model_comb_dat |>
    summarise(overlap = mean(both_avail) * (n() / n_obs)) |>
    pull()
  
  
  if(avail_overlap < avail_overlap_threshold){
    
    dist_dat <- data.frame(
      model1 = model_combs[1], 
      model2 = model_combs[2],
      forecast_date = rep(unique(subdat$forecast_date), each = 4),
      horizon = rep(1:4, times = length(unique(subdat$forecast_date))),
      dist = NA,
      avail_overlap = avail_overlap
    )
    
  } else {
    
    dist_dat <- model_comb_dat |>
      filter(!is.na(get(mod1)),
             !is.na(get(mod2))) |>
      dplyr::group_by(forecast_date, horizon) |>
      #compute distance metric
      dplyr::summarise(dist = dist_fun(
        get(mod1), get(mod2), quantile
      )) |>
      dplyr::ungroup() |>
      dplyr::mutate(model1 = model_combs[1],
                    model2 = model_combs[2]) 
    
    
    #fill dist_dat with NAs at unavailable forecast_dates
    na_dat <- data.frame(
      model1 = model_combs[1], 
      model2 = model_combs[2],
      forecast_date = rep(unique(subdat$forecast_date), each = 4),
      horizon = rep(1:4, times = length(unique(subdat$forecast_date)))
    )
    
    avail_dat <- data.frame(
      model1 = model_combs[1],
      model2 = model_combs[2],
      avail_overlap = avail_overlap
    )
    
    #join with dist_dat (unavailable dates are now explicitly NAs)
    dist_dat <- dplyr::full_join(
      dist_dat, na_dat, 
      by = c("forecast_date", "model1", "model2", "horizon")) |>
      dplyr::arrange(model1, model2, forecast_date) |>
      dplyr::relocate(model1, model2) |>
      left_join(avail_dat, by = c("model1", "model2"))
    
  }
  return(dist_dat)
  
}


 
#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description Helper function to compute the Wasserstein 2 metric
#'         for discrete quantiles
#'
#' @param q_F quantiles of distribution F
#' @param q_G quantiles of distribution G
#' @param tau quantile levels of F and G
#' 
#' @return Wasserstein distance 
 
#' @export       
       
wasserstein_dist <- function(q_F, q_G, tau){
  return(sum(q_F-q_G)^2)
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function to compute the Cramer distance. 
#' Minimally adapted from: https://github.com/reichlab/covidHubUtils/blob/master/R/calc_cramers_dist_unequal_space.R
#' 
#' @param q_F quantiles of distribution F
#' @param q_G quantiles of distribution G
#' @param tau quantile levels of F and G
#' 
#' @return Cramer distance ()

#' @export       


cramers_dist <- function(q_F , q_G, tau) {
    # check rules
    # check quantile order
    q_F_ordered <- sort(q_F)
    q_G_ordered <- sort(q_G)
    
    if (sum(q_F != q_F_ordered)>0) {
      warning("q_F has been re-ordered to correspond to increasing probability levels")
    }
    if (sum(q_G != q_G_ordered)>0) {
      warning("q_G has been re-ordered to correspond to increasing probability levels")
    }
    # check probability level order
    tau_ordered <- sort(tau)
    if (sum(tau != tau_ordered)>0) {
      warning("tau has been sorted to in an increasing order")
    }
    
    # check conditions
    if (length(q_F_ordered) != length(tau_ordered)) {
      print(paste("the lengths of q_F", length(q_F_ordered), "and tau", length(tau_ordered)))
      stop("The lengths of q_F_ordered and tau_ordered need to be equal")
    }
    if (length(q_G_ordered) != length(tau_ordered)) {
      stop("The lengths of q_G_ordered and tau_ordered need to be equal")
    }
    if (sum(tau_ordered<=1)!=length(tau_ordered)|sum(tau_ordered>=0)!=length(tau_ordered)) {
      stop("The values of tau_ordered have to be between 0 and 1")
    }
    if (length(q_F_ordered) != length(q_G_ordered)) {
      message("The lengths of q_F_ordered and q_G_ordered are not equal")
    }
    N <- length(q_F_ordered)
    M <- length(q_G_ordered)
    # pool quantiles:
    q0 <- c(q_F_ordered, q_G_ordered)
    # indicator whether the entry is from F or G
    q <- q0[order(q0)]
    tf <- unlist(sapply(q, function(x) ifelse(x %in% q_F_ordered,tau_ordered[which(x == q_F_ordered)],0)))
    tg <- unlist(sapply(q, function(x) ifelse(x %in% q_G_ordered,tau_ordered[which(x == q_G_ordered)],0)))
    diffs_q <- diff(q)
    # probability level vectors
    tau_F_v <- cummax(tf)
    tau_G_v <- cummax(tg)
    
    cvm <-
      sum(((tau_F_v[1:(N + M) - 1] - tau_G_v[1:(N + M) - 1]) ^ 2) * diffs_q)
    
    return(cvm)
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function to transform output from model_dist
#' 
#' @param model_dist_mat single matrix from model_dist output (list)
#' 
#' @return matrix as data.table

#' @export   


model_dists_to_dt <- function(model_dist_mat){
  
  #for changing to factor
  model_order <- colnames(model_dist_mat)
  
  #transform to data.frame
  model_dists_dt <- model_dist_mat |>
    data.table::data.table(keep.rownames = TRUE) |>
    #id is first column
    data.table::melt(id = 1) |>
    dplyr::rename(model1 = rn,
                  model2 = variable,
                  distance = value) |>
    dplyr::mutate(model1 = factor(model1, 
                                  levels = model_order),
                  model2 = factor(model2, 
                                  levels = model_order))
  
  return(model_dists_dt)
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function to transform output from model_dist
#' 
#' @param model_dist_mat single matrix from model_dist output (list)
#' 
#' @return matrix as data.table

#' @export   

model_dists_to_mat <- function(model_dist_dt){
  
  model_dist_dt_rep <- model_dist_dt |>
    rename(model1 = model2,
           model2 = model1)
  
  model_dist_dt <- rbind(
    model_dist_dt_rep, model_dist_dt
  )
  
  colrownames <- sort(unique(unlist(model_dist_dt[1:2])))
  # construct 0 matrix of correct dimensions with row and column names
  model_dist_mat <- matrix(NA, length(colrownames), 
                  length(colrownames), 
                  dimnames = list(colrownames, colrownames))
  
  # fill in the matrix with matrix indexing on row and column names
  model_dist_mat[as.matrix(model_dist_dt[c("model1", "model2")])] <- 
    model_dist_dt[[names(model_dist_dt)[3]]]
  
  return(model_dist_mat)
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function to give forecast dates in sliding window
#' 
#' @param curr_date the current forecast date
#' @param window size of the sliding window
#' @param incl should current date be in included in sliding window (default is
#' FALSE, which should be used if current date's information has not realized yet.
#' for e.g. distance calculations between forecasts, can be set to TRUE)
#' 
#' @return vector with dates in sliding window

fc_dates_window <- function(curr_date, window, incl = FALSE){
  
  curr_date <- as.Date(curr_date)

  if(incl){
    end_date <- curr_date
    start_date <- end_date - window * 7
  } else {
    end_date <- curr_date - 7
    start_date <- end_date - (window-1) * 7
  }
  

  all_dates <- seq.Date(from = start_date, 
                        to = end_date, 
                        by = 7)
  
  return(all_dates)
}

#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function for assessment of model types
#' Includes coverage, bias 
#' 
#' @param hub_data data from the European forecast hub
#' @param coverage_ranges PI ranges to compute coverage for
#' @param avail_threshold  threshold of availability for models to be included
#' @param adjust_avail should each model or each forecast date - location be weighted
#' equally (default: TRUE, i.e. forecast date - location)
#' 
#' @return data.table

#' @export   
overall_assessment_model_types <- function(hub_data,
                                           coverage_ranges = c(50,90),
                                           avail_threshold = 0,
                                           adjust_avail = TRUE){
  #extract model types before scoring
  model_types <- hub_data |>
    select(model, model_type) |>
    distinct()
  
  
  ##Coverage
  ##adding coverage at level of model_type ensures that models are not
  #all weighted equally, but by their level of availability
  #i.e. each forecast unit is weighted equally
  #other scores are included as usual
  all_scores <- hub_data |>
    filter(availability >= avail_threshold,
           !model_type %in% c("other", "ensemble", "baseline")) |>
    select(all_of(specs$su_cols)) |> #only keep in columns defining single forecast unit
    score() |>
    left_join(model_types, by = c("model")) #|> #join back model type info
  
  
  if(adjust_avail){
    all_scores <- all_scores |>
      add_coverage(ranges = coverage_ranges,
                   by = c("model", "horizon", "target_type")) |>
      summarise_scores(by = c("model_type", "horizon", "target_type", 
                              "location", "forecast_date" )) |>
      summarise_scores(by = c("model_type", "target_type", "horizon"))
  } else {
    all_scores <- all_scores |>
      add_coverage(ranges =  coverage_ranges,
                   by = c("model_type", "horizon", "target_type")) |>
      summarise_scores(by = c("model_type", "target_type", "horizon"))
  }
    
  
  return(all_scores)
  
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function for decomposition of WIS for model types
#' 
#' @param hub_data data from the European forecast hub
#' @param coverage_ranges PI ranges to compute coverage for
#' @param avail_threshold  threshold of availability for models to be included
#' @param adjust_avail should each model or each model - forecast date - location instance
#' be weighted equally (default: TRUE, i.e. equal weight of each model - forecast date - location
#' instance; this prevents models that have low availability to influence average scores too much)
#' 
#' @return data.table (long format) with decomposed WIS at level of model types

#' @export 
compute_decomp_scores <- function(data, 
                                  adjust_avail = TRUE){
  
  decomp_scores  <- hub_data |>
    filter(!model_type %in% c("other", "ensemble", "baseline")) |>
    select(all_of(c(specs$su_cols, "model_type"))) |>
    score()
  
  if(adjust_avail){
    decomp_scores <- decomp_scores |>
      summarise_scores(by = c("model_type", 
                              "horizon", "target_type", "location", "forecast_date")) |>
      summarise_scores(by = c("model_type", 
                              "horizon", "target_type"))
  } else {
    decomp_scores <- decomp_scores |>
      summarise_scores(by = c("model_type", 
                              "horizon", "target_type"))
  }
  
  #get data into long format for plotting
  decomp_scores <- decomp_scores |>
    dplyr::select(model_type, horizon, target_type,
                  overprediction, underprediction, dispersion) |>
    dplyr::ungroup() |>
    dplyr::arrange(model_type) |>
    data.table::melt(id.vars = c("model_type", 
                                 "horizon", "target_type"))
  
}


#this is a stupid function because I'm stupid
add_tt_and_dates <- function(add_model_dat,
                             ens_dat){
  
  #find breaks in date (switch from idx 50 to 1)
  matcher <- add_model_dat |>
    mutate(lagg = idx - lag(idx),
           lagg = ifelse(is.na(lagg), 0, lagg),
           identi = as.numeric(lagg < 0),
           groupp = cumsum(identi)) |>
    group_by(groupp) |>
    summarise(count = n())
  
  #how many observations does each target_type have at each forecast_date
  case_deaths_counts <- ens_dat |>
    select(target_type, forecast_date) |>
    distinct()

  #two dataframes that have info about combination of fc_date and number of observations
  #for each target_type
  nrow_cases <- nrow(case_deaths_counts[case_deaths_counts$target_type == "Cases",])
  nrow_deaths <- nrow(case_deaths_counts[case_deaths_counts$target_type == "Deaths",])
  matcher_cases <- matcher[1:nrow_cases,]
  matcher_deaths <- matcher[(nrow_cases+1):nrow(matcher),]
  

  #lapply function that reps the respective forecast_date as many times as it is
  #available in ensemble data 
  fcdates <- case_deaths_counts |>
    split(by = "target_type")

  fcdates_cases <- lapply(seq_along(fcdates[[1]]$forecast_date), function(k)
    rep(fcdates[[1]]$forecast_date[k], times = matcher_cases$count[k])) |>
    unlist() |>
    as.Date(origin = "1970-01-01") |>
    data.table() |>
    mutate(target_type = "Cases") |>
    rename(forecast_date = V1)
  fcdates_deaths <- lapply(seq_along(fcdates[[2]]$forecast_date), function(k)
    rep(fcdates[[2]]$forecast_date[k], times = matcher_deaths$count[k])) |>
    unlist() |>
    as.Date(origin = "1970-01-01") |>
    data.table() |>
    mutate(target_type = "Deaths") |>
    rename(forecast_date = V1)

  
  if(FALSE){
    #can be used alternatively if one target_type has same number of observations
    #across the whole dataset
    fcdates_deaths <- rep(fcdates[[2]]$forecast_date, each = 3700) |>
      as.Date(origin = "1970-01-01") |>
      data.table() |>
      mutate(target_type = "Deaths") |>
      rename(forecast_date = V1)
  }
  
  extra_cols <- rbind(fcdates_cases, fcdates_deaths)
  
  return(cbind(add_model_dat, extra_cols))
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description Helper function to evaluate different ensemble strategies
#' 
#' @param target ensemble data to evaluate score against current
#' @param current comparison data
#' @param su_cols columns for scoringutils
#' @param scoring_fun which scoring function to evaluate
#' @param strat_by unit of a single forecast
#' @param return_anti_join if target's and current's targets don't match (function
#' gives and error), should setdiff between the two be returned
#' 
#' @return data.table (long format) with relative scores

#' @export 
make_eval <- function(target,
                      current,
                      su_cols,
                      scoring_fun = "interval_score",
                      strat_by = c("model", "target_type","location", 
                                   "horizon", "forecast_date"),
                      return_anti_join = FALSE){
  
  #check if there are multiple models in any of the datasets
  if(length(unique(target$model))!=1){
    mods <- paste0(unique(target$model))
    message(paste0("There is more than one model in target: ", paste(mods, collapse = ", ")))
    stop("Can only pass one model per dataframe.")
  }
  if(length(unique(current$model))!=1){
    mods <- paste0(unique(current$model))
    message(paste0("There is more than one model in current: ", paste(mods, collapse = ", ")))
    stop("Can only pass one model per dataframe.")
  }
  
  
  #####check if both models predict for all the same instances#######
  all_inst_target <- target |>
    dplyr::select(all_of(strat_by[-1])) |> #remove model from strat_by
    dplyr::distinct()
  all_inst_current <- current |>
    dplyr::select(all_of(strat_by[-1])) |> #remove model from strat_by
    dplyr::distinct()
  
  #perform anti join to find instances that are not in intersection
  not_in_current <- dplyr::anti_join(all_inst_target, all_inst_current, by = strat_by[-1])
  not_in_target <- dplyr::anti_join(all_inst_current, all_inst_target, by = strat_by[-1])
  
  if(!nrow(not_in_current) == 0){
    message("There are instances in target that are not in current.")
    if(return_anti_join){
      message("Returning these instances.")
      return(not_in_current)
    }
    stop("Relative WIS cannot be reliably computed")
  }
  if(!nrow(not_in_target) == 0){
    message("There are instances in current that are not in target.")
    if(return_anti_join){
      message("Returning these instances.")
      return(not_in_target)
    }
    stop("Relative WIS cannot be reliably computed")
  }
  
  #additionally check for nrow (could e.g. be missing quantiles, 
  #although I have absolutely no idea how that would have happened)
  if(!identical(nrow(target), nrow(current))){
    message(paste0("Target has ", nrow(target), " rows, current has ", nrow(current)))
    stop("Relative WIS cannot be reliably computed")
  }
  
  
  
  ########Scoring#########
  #score target
  target_score <- target |>
    dplyr::select(all_of(su_cols)) |> #remove redundant cols before scoring
    scoringutils::score() |>
    scoringutils::summarise_scores(by = strat_by) |>
    dplyr::select(all_of(c(strat_by, scoring_fun))) |>
    dplyr::rename(target_val = scoring_fun)
  
  #score current
  current_score <- current |>
    dplyr::select(all_of(su_cols)) |> #remove redundant cols before scoring
    scoringutils::score() |>
    scoringutils::summarise_scores(by = strat_by) |>
    dplyr::select(all_of(c(strat_by, scoring_fun))) |>
    dplyr::rename(current_val = scoring_fun) |>
    dplyr::select(-model) #remove before joining
  
  
  #join scores and compute relative score
  joined_scores <- target_score |>
    dplyr::left_join(current_score, by = strat_by[-1]) |> #remove model from strat_by
    dplyr::mutate(rel_score = target_val/current_val)
  
  return(joined_scores)
  
}
