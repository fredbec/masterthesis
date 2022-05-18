#' Computes coverage per model and location over entire observation time
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param alldat should full data be returned with extra column for 
#'              coverage added (the default) or only a dataframe with
#'              the coverage data (columns are models, location,
#'              target_type)
#'        
model_coverage <- function(data, alldat = TRUE){
  
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
    select(model, forecast_date, target_type, location) |>
    distinct() |>
    mutate(is_present = 1)
  
  #merge two dataframes to compute coverage
  coverage <- merge(all_combs, actual_combs,
                    by = c("model", "forecast_date", "target_type", "location"), 
                    all.x = TRUE) |>
    mutate(is_present = as.numeric(!is.na(is_present)),
           model = as.character(model)) |>
    group_by(model, target_type, location) |>
    summarise(coverage = mean(is_present)) |>
    ungroup()
  
  if(alldat){
    data <- merge(data, coverage, 
                  by = c("model", "target_type", "location")) |>
      setkey(model)
    return(data)
    
  } else {
    return(coverage)
  }
  
}
