source(here("code", "fun_make_ensemble.R"))

#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#' Uses fun_make_ensemble.R
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param nmods 
#' @param samples how many random samples of models to draw 

leaveout_ensemble <- function(data,
                              nmods = c(5, 7, 10),
                              samples = 10,
                              seed = 32,
                              excl = c("EuroCOVIDhub-baseline",
                                       "EuroCOVIDhub-ensemble"),
                              cvg_threshold = NULL){
  
  set.seed(seed)
  
  if(!is.null(cvg_threshold)){
    stop("flexible definition of coverage threshold is not implemented (yet)")
  }
  
  #to obtain models that are above cvg threshold and not in excl
  getmodels <- function(data){
    #print(data)
    models <- data |>
      filter(cvg_incl == 1) |>
      (\(x) unique(x$model))() |>
      (\(x) x[-which(x %in% excl)])()
    
    #check if there are still ensembles
    is_ensemble <- grepl(".*ensemble.*", models)
    if(any(is_ensemble)){
      warning(paste0("There seems to be at least one ensemble (\"" , 
                     paste(models[is_ensemble], collapse = "\", \""), 
                     "\") that is not in the list of excluded models. Is this intended?"))
    }
    
    return(models)
  }
  
  #make model list per country
  per_loc <- split(data, f = data$location)
  models_per_loc <- lapply(per_loc, FUN = getmodels)
    
  
  #locs <- unique(data$location)
  locs_list <- c(list(locs), lapply(locs, function(x) c(x)))
  
  #############***#########
  #for now: only individual locations:
  locs_list <- lapply(locs, function(x) c(x))
  
  score_tabs <- list()
  result_table <- NULL
  
  for (loc in locs_list){
    for (nmod in nmods){
      for (i in 1:samples){
        
        print(loc)
        models <- sample(models_per_loc[[loc]], 5)
        
        #entry for location column
        loc_col <- ifelse(length(loc)>1, "all", loc)
        
        #make ensembles with current set of models
        ensembles <- data |>
          filter(location %in% loc,
                 model %in% models) |>
          make_ensemble(summary_function = mean) |>
          make_ensemble(summary_function = median,
                        extra_excl = c("mean_ensemble")) |>
          filter(model %in% c("median_ensemble",
                              "mean_ensemble")) |>
          mutate(location = loc_col,
                 nmod = nmod)
        
        
        #score resulting ensembles
        score_tabs[[i]] <- ensembles |>
          score() |>
          summarise_scores(by = c("model", "target_type", 
                                  "location", "nmod")) 
        
        #print(score_tabs)
        
      }
      #average over all tables for set of nmod + loc 
      result_table <- rbind(result_table,
                            rbindlist(score_tabs)[,lapply(.SD,mean), 
                                                  list(model, target_type, 
                                                       location, nmod)]
      )
    }
    
    #print(result_table)
  }
  
  return(result_table)
  
 
}