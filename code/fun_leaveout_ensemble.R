#' Assesses ensemble performance by iteratively sampling given numbers of 
#' model for the (mean/median) ensemble 
#' 
#' Uses fun_make_ensemble.R
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param nmods 
#' @param samples how many random samples of models to draw 

leaveout_ensemble <- function(data,
                              nmods = c(5, 10, 15),
                              samples = 10,
                              seed = 32,
                              excl = c("EuroCOVIDhub-baseline",
                                       "EuroCOVIDhub-ensemble")){
  
  set.seed(seed)
  
  #gets models that are not in excl
  getmodels <- function(data, loc){
    models <- data |>
      filter(location == loc) |>
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
  
  locs <- unique(data$location)

  results <- NA
  for (loc in locs){
    for (i in 1:samples){
      
      models <- getmodels(hub_data, "DE") |>
        sample(5)
      print(models)
      
    }
  }
  
  return("done")
}