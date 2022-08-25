#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' @import scoringutils
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
#' 
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
#' @description Helper function for pairwise comparison of model types
#' 
#' @param data data from the European forecast hub
#' @param su_cols cols defining unit of single forecast
#' @param res_before_pw_comp at what level to average across model_types
#' It is important that forecast_date, location are included here, otherwise 
#' results will be skewed
#' @param end_res what level of stratification end result should have 
#' default: model_type (it's only called more for scoringutils reasons), horizon, target_type
#' 
#' @return data.table

#' @export   
pw_comp_by_model_type <- function(data, 
                                  su_cols,
                                  res_before_pw_comp = 
                                    c("model_type", "forecast_date", "quantile",
                                      "location", "target_type", "horizon"),
                                  end_res = c("model", "horizon", "target_type")){
  
  #for merging after scoring 
  model_types <- data |>
    dplyr::select(model, model_type) |>
    dplyr::distinct()
  
  
  #this summarizes across models, so get score at level of model_type
  all_scores <- data |>
    dplyr::select(all_of(su_cols)) |>
    scoringutils::score() |>
    dplyr::left_join(model_types, by = "model") |> #join model_type back after scoring
    scoringutils::summarise_scores(by = res_before_pw_comp)
  
  pw_comp <- all_scores |>  
    #exclude ensemble from scoring
    dplyr::filter(!model_type == "ensemble") |>
    dplyr::rename(model = model_type) |>
    scoringutils::pairwise_comparison(by = end_res,
                                      baseline = "baseline") 
  
  
  return(pw_comp)
}
