###preamble function definition
#`%nin%` = Negate(`%in%`)


#' Ensemble builder function for data from the European Forecast Hub
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param summary_function function to build ensemble from (median by default)
#' @param model_name name for the resulting model. By default, makes a name based
#'                   on summary_function
#' @param excl models to exclude from ensemble (by default the official ensemble
#'            and the baseline)
#' @param extra_excl extra models to exclude, apart from excl
#' @param strat how to stratify, i.e. unit defining a single forecast
#' @param extra_vars any extra (redundant) variables that should stay in the data

#make ensemble by summary
make_ensemble <- function(data, 
                          summary_function = median,
                          model_name = NULL,
                          excl = c("EuroCOVIDhub-baseline", 
                                   "EuroCOVIDhub-ensemble"),
                          extra_excl = NULL,
                          strat = c("location", "forecast_date", "quantile",
                                    "horizon", "target_type"),
                          extra_vars = c("target_end_date", "n", 
                                         "population", "cvg_incl")){
  
  #extract function name to make model name
  if(is.null(model_name)){
    model_name <- deparse(summary_function) |>
      paste(collapse = " ") |>
      (\(x) gsub(".*\"(.*)\".*", "\\1", x = x))() |>
      paste0("_ensemble")
  }
  
  if(!is.null(extra_excl)){
    excl <- c(excl, extra_excl)
  }

  #check if any ensembles in included models (this should generally not be the case)
  models <- data |>
    filter(!(model %in% excl)) |>
    (\(x) unique(x$model))()

  is_ensemble <- grepl(".*ensemble.*", models)

  if(any(is_ensemble)){
    warning(paste0("There seems to be at least one ensemble (\"" , 
                  paste(models[is_ensemble], collapse = "\", \""), 
                  "\") that is not in the list of excluded models. Is this intended?"))
  }
  
  #extract extra columns before summarising (merge again later)
  extra_cols <- data |>
    select(all_of(c(strat, extra_vars))) |>
    distinct()
  
  
  #make mean/median forecast
  ensemble <- data |>
    group_by(across(all_of(strat))) |>
    filter(!(model %in% excl)) |>
    summarise(prediction = summary_function(prediction),
              tvsd = sd(true_value),
              true_value = mean(true_value), #this is not totally clean, so check further down
              .groups = 'drop') |> 
    mutate(model = model_name,
           model_type = "ensemble") |> #for appending to original data
    select(model, everything()) |> #reordering
    merge(extra_cols, by = strat) #add back extra cols
  
  
  #check if true values were unique before summarising
  if(any(ensemble$tvsd!=0)){
    stop("there are multiple true values for one instance")
  } else {
    ensemble$tvsd <- NULL
  }
  
  data_with_ens <- rbind(data, ensemble)
  
  return(data_with_ens)
}
