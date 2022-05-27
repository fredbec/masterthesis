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
                              nmods = c(5, 7, 10, 100),
                              samples = 100,
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
        
        if (nmod == 100){
          models <- models_per_loc[[loc]]
          nmod <- "all"
        }
        
        #print(loc)
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

#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#' Uses fun_make_ensemble.R
#'
#' @param score_data score table output from leaveout_ensemble function
#' @param score which score to evaluate (needs to match column names as 
#'              induced by scoringutils' score() function)
#'              default is interval_score
#' @param givedata should relative score data be returned instead of plot
#' @param title optional, alternative title
#' @param saveplot should plot be saved in pdf format?
#' @param path where to save plot to 

plot_leaveout <- function(score_data, score = "interval_score", 
                          givedata = FALSE, title = NULL, 
                          saveplot = TRUE,
                          path = here("plots", "leaveout_ensemble.pdf")
                          ){
  
  #divide all values by those of ensemble using all models
  rel_data <- score_data |>
    filter(nmod == "all") |>
    select(model, target_type, 
           location, interval_score) |> #dataset of only "all", to merge 
    rename(ref = all_of(score)) |>
    merge(score_data, 
          by = c("model", "target_type", "location")) |>
    mutate(relative_score = get(score)/ref) |> 
    select(model, target_type, location, nmod,
           relative_score, all_of(c(score)))
  
  
  if(givedata){
    return(rel_data)
  }
  
  
  if(is.null(title)){
    title <- "Relative score of ensemble using subset of models"
  }
  
  plot <- rel_data |>
    filter(nmod != "all") |> #all obs are one
    mutate(nmod = as.numeric(nmod)) |>
    ggplot(aes(x = nmod, y = relative_score,
               color = model, linetype = target_type)) +
    geom_path() +
    facet_wrap(~ location) + 
    labs(x = "Number of models sampled for ensemble",
         y = "Score relative to full ensemble",
         title = title)
    
  print(plot)
    
  if(saveplot){
    ggsave(
      filename = path,
      width = 12, height = 9
    )
  }
  
}
