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

