###strat by target_typ!!!!!!!!!

model_coverage <- function(data, loc, alldat = TRUE){
  #print(target_type)
  data <- data |>
    filter(location == loc)
  
  models <- unique(data$model)
  dates <- unique(as.Date(data$forecast_date))
  targets <- unique(data$target_type)
  
  #data frame of all possible instances (combinations of model, fc_date)
  all_combs <- expand.grid(model = models, 
                           forecast_date = dates,
                           target_type = targets)
  
  #instances that are actually in the data
  actual_combs <- data |>
    select(model, forecast_date, target_type) |>
    distinct() |>
    mutate(is_present = 1)
  
  #merge two dataframes to compute coverage
  coverage <- merge(all_combs, actual_combs,
                    by = c("model", "forecast_date", "target_type"), 
                    all.x = TRUE) |>
    mutate(is_present = as.numeric(!is.na(is_present)),
           model = as.character(model)) |>
    group_by(model, target_type) |>
    summarise(coverage = mean(is_present)) |>
    ungroup()
  
  if(alldat){
    data <- merge(data, coverage, 
                  by = c("model", "target_type")) |>
      setkey(model)
    return(data)
  } else {
    return(coverage)
  }
  
}
