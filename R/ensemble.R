#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#' Ensemble builder function for data from the European Forecast Hub
#' By default, builds ensemble based on all models except the baseline and Hub 
#' ensemble models. More models to exclude or, for the reverse approach, a list 
#' of models to include may be supplied 
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param summary_function function to build ensemble from (median by default)
#' @param model_name name for the resulting model. By default, makes a name based
#'                   on summary_function
#' @param excl models to exclude from ensemble (by default the official ensemble
#'            and the baseline)
#' @param extra_excl extra models to exclude, apart from excl
#' @param incl explicit list of models that should be included in the ensemble
#' @param strat how to stratify, i.e. unit defining a single forecast
#' @param extra_vars any extra (redundant) variables that should stay in the data
#' @param avail_threshold minimum availability for models to be included in 
#'             the ensemble
#' 
#' @return 
#' a data.table object that contains the ensemble forecasts in addition to the 
#' models that are already in data
#' 
#' @export

#make ensemble by summary
make_ensemble <- function(data, 
                          summary_function = median,
                          model_name = NULL,
                          excl = c("EuroCOVIDhub-baseline", 
                                   "EuroCOVIDhub-ensemble"),
                          extra_excl = NULL,
                          incl = NULL,
                          strat = c("location", "forecast_date", "quantile",
                                    "horizon", "target_type"),
                          extra_vars = c("target_end_date", "n", 
                                         "population"),
                          avail_threshold = NULL){
  
  #extract function name to make model name
  if(is.null(model_name)){
    model_name <- deparse(summary_function) |>
      paste(collapse = " ") |>
      (\(x) gsub(".*\"(.*)\".*", "\\1", x = x))() |>
      paste0("_ensemble")
  }
  
  
  #######make model vector based on incl/excl arguments#####
  if(!is.null(extra_excl)){
    excl <- c(excl, extra_excl)
  }
  
  models <- masterthesis::getmodels(data, 
                                    excl = excl, 
                                    incl = incl, 
                                    avail_threshold = avail_threshold)
  
  
  #check if any ensembles in models (this should in general not be so)
  is_ensemble <- grepl(".*ensemble.*", models)
  
  if(any(is_ensemble)){
    warning(paste0("There seems to be at least one ensemble (\"" , 
                   paste(models[is_ensemble], collapse = "\", \""), 
                   "\") that is not in the list of excluded models. Is this intended?"))
  }
  
  
  #############make mean/median forecast################
  
  #extract extra columns before summarising (merge again later)
  extra_cols <- data |>
    dplyr::select(all_of(c(strat, extra_vars))) |>
    dplyr::distinct()
  
  ensemble <- data |>
    dplyr::group_by(across(all_of(strat))) |>
    dplyr::filter(model %in% models) |>
    dplyr::summarise(prediction = summary_function(prediction),
                     tvsd = stats::sd(true_value),
                     true_value = mean(true_value), #this is not totally clean, so check further down
                     .groups = 'drop') |> 
    dplyr::mutate(model = model_name,
                  availability = 1,
                  model_type = "ensemble") |> #for appending to original data
    dplyr::select(model, everything()) |> #reordering
    merge(extra_cols, by = strat) #add back extra cols
  

  #check if true values were unique before summarising
  if(any(is.na(ensemble$tvsd))){
    warning("only one model in the ensemble")
    ensemble$tvsd <- NULL
  } else {
    if(any(ensemble$tvsd!=0)){
      warning("there are multiple true values for one instance")
    } else {
      ensemble$tvsd <- NULL
    }
  }
  data_with_ens <- rbind(data, ensemble)
  
  return(data_with_ens)
}


#'  @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' 
#' @description 
#' Minimal Ensemble builder function for data from the European Forecast Hub
#' Used with make_ensemble, has some features removed. Not recommended for other uses.
#' Different from other version: takes reduced dataset, only returns ensemble data
#' and not other models
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param summary_function function to build ensemble from (median by default)
#' @param model_name name for the resulting model. By default, makes a name based
#'                   on summary_function
#' @param excl models to exclude from ensemble (by default the official ensemble
#'            and the baseline)
#' @param extra_excl extra models to exclude, apart from excl
#' @param incl explicit list of models that should be included in the ensemble
#' @param strat how to stratify, i.e. unit defining a single forecast
#' @param extra_vars any extra (redundant) variables that should stay in the data
#' @param avail_threshold minimum availability for models to be included in 
#'             the ensemble
#' 
#' @return 
#' a data.table object that contains the ensemble forecasts 
#' @export

#make ensemble by summary
make_ensemble_minimal <- function(
    data, 
    which_ens = NULL,
    excl = c("EuroCOVIDhub-baseline", 
             "EuroCOVIDhub-ensemble"),
    extra_excl = NULL,
    strat = c("location", "forecast_date", "quantile",
              "horizon", "target_type")){
  
  models <- unique(data$model)

  #check if any ensembles in models (this should in general not be so)
  is_ensemble <- grepl(".*ensemble.*", models)
  is_baseline <- grepl(".*baseline.*", models)
  
  if(any(is_ensemble)|any(is_baseline)){
    warning(paste0("There seems to be at least one ensemble or baseline model (\"" , 
                   paste(models[is_ensemble], collapse = "\", \""), 
                   "\") that is not in the list of excluded models. Is this intended?"))
  }
  
  if(is.null(which_ens)){
    ensemble_mean <- data |>
      dplyr::group_by(across(all_of(strat))) |>
      dplyr::filter(model %in% models) |>
      dplyr::summarise(prediction = mean(prediction),
                       true_value = mean(true_value), #this is not totally clean, so check further down
                       .groups = 'drop') |> 
      dplyr::mutate(model = "mean_ensemble") |>
      dplyr::select(model, everything()) |>#reordering
      data.table()
    
    ensemble_median <- data |>
      dplyr::group_by(across(all_of(strat))) |>
      dplyr::filter(model %in% models) |>
      dplyr::summarise(prediction = median(prediction),
                       true_value = mean(true_value), #this is not totally clean, so check further down
                       .groups = 'drop') |> 
      dplyr::mutate(model = "median_ensemble") |>
      dplyr::select(model, everything()) |>#reordering
      data.table()
  
    ensemble <- rbind(ensemble_mean,
                      ensemble_median)
    return(ensemble)
  } else if (which_ens == "mean_ensemble"){
    ensemble_mean <- data |>
      dplyr::group_by(across(all_of(strat))) |>
      dplyr::filter(model %in% models) |>
      dplyr::summarise(prediction = mean(prediction),
                       true_value = mean(true_value), #this is not totally clean, so check further down
                       .groups = 'drop') |> 
      dplyr::mutate(model = "mean_ensemble") |>
      dplyr::select(model, everything()) |>#reordering
      data.table()
    return(ensemble_mean)
  } else if (which_ens == "median_ensemble"){
    ensemble_median <- data |>
      dplyr::group_by(across(all_of(strat))) |>
      dplyr::filter(model %in% models) |>
      dplyr::summarise(prediction = median(prediction),
                       true_value = mean(true_value), #this is not totally clean, so check further down
                       .groups = 'drop') |> 
      dplyr::mutate(model = "median_ensemble") |>
      dplyr::select(model, everything()) |>#reordering
      data.table()
    return(ensemble_median)
  } else {
    print(which_ens)
  }
  
  return(ensemble)
}
