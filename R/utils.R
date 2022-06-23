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
#'       


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
#' Helper function that computes the discrete Wasserstein 2-metric
#' 
#' @param data data (subset or full) from the European Forecast hub
#' @param avail_threshold minimum availability for a model to be considered
#' @param excl models to exclude from the calculation
#' 
#' @return either a data.frame with only the models and their respective
#'        availability, or the original data.table including and extra column
#'        with the availability values of the respective models (the default)
#'
#'       

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
  
  #each list element is the same initial matrix
  model_matrices <- lapply(model_matrices, function(x) 
    matrix(ncol = length(models),
           nrow = length(models),
           dimnames = list(models, models)
           ))
  
  
  #loop over each combination of target and location
  for (comb in names(model_matrices)){ 
    #split up combination of location and target
    filters <- strsplit(comb, split = "[.]")[[1]]
    
    subdat <- data |>
      dplyr::filter(location == filters[1],
                    target_type == filters[2])
    
    model_combs <- combn(unique(subdat$model), 2) |> t()
    
    for (i in 1:nrow(model_combs)){
      
      #for easier readability in pipe
      #these are the column names for the 2 models' preds after reshape
      mod1 <- paste0("prediction.", model_combs[i,1])
      mod2 <- paste0("prediction.", model_combs[i,2])

      model_comb_dat <- subdat |>
        dplyr::filter(model %in% model_combs[i,]) |> #get two relevant models
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
        model_matrices[[comb]][model_combs[i,1], 
                               model_combs[i,2]] <- NA

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
          #dplyr::summarise(dist = mean(ddist)) |>
          #take average over all quantiles and forecast_dates
          dplyr::summarise(avg_dist = mean(dist)) |>
          dplyr::pull()
        
        model_matrices[[comb]][model_combs[i,1], 
                               model_combs[i,2]] <- dist_val
        
      }

    }
    
    #return(model_matrices[[comb]])
    #remove models with no predictions for each target-location 
    model_matrices[[comb]] <- 
      model_matrices[[comb]][unique(subdat$model), 
                             unique(subdat$model)]
    
    if (normalize){
      maxval <- max(model_matrices[[comb]], na.rm = TRUE)
      model_matrices[[comb]] <- 
        model_matrices[[comb]] / maxval
      
    }
    
    #fill lower triangular with upper triangular values for symmetric matrix
    if (return_symmetric){
      model_matrices[[comb]][lower.tri(model_matrices[[comb]])] <- 
        t(model_matrices[[comb]])[lower.tri(model_matrices[[comb]])]
    }
  }
  return(model_matrices)  
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#'
#' @param q_F quantiles of distribution F
#' @param q_G quantiles of distribution G
#' @param tau quantile levels of F and G
#' 
#' @return Wasserstein distance 
#'       
       
wasserstein_dist <- function(q_F, q_G, tau){
  return(sum(q_F-q_G)^2)
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @description 
#'
#' Function to compute the Cramer distance. 
#' Minimally adapted from: https://github.com/reichlab/covidHubUtils/blob/master/R/calc_cramers_dist_unequal_space.R
#' 
#' @param q_F quantiles of distribution F
#' @param q_G quantiles of distribution G
#' @param tau quantile levels of F and G
#' 
#' @return Cramer distance ()
#'       

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