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