all_combs_ensemble2 <- function(data,
                                model_dist_data,
                                nmod = 3, 
                                window = 5,
                                init_weeks = 5,
                                avail_threshold = 0.5,
                                avail_overlap_threshold = 0.1,
                                excl = c("EuroCOVIDhub-baseline",
                                         "EuroCOVIDhub-ensemble")){
  
  #filter models from data that are in excl or below
  #availability threshold
  data <- data |>
    filter(!model %in% excl,
           availability >= avail_threshold)
  
  
  #make all combinations of location+target (for looping)
  locs <- unique(data$location)
  targets <- unique(data$target_type)
  combos <- paste(rep(locs, each = length(targets)), 
                  targets, sep = ".")
  
  
  #get all forecast_dates in data
  fc_dates <- data |>
    select(forecast_date) |>
    distinct() |>
    pull() |>
    sort()
  
  #init result container for all ensemble predictions
  all_ensemble_data <- NULL 

  #loop over all loc+target combinations
  for(comb in combos){

    #get location and target
    filters <- strsplit(comb, split = "[.]")[[1]]
    loc <- filters[1]
    target <- filters[2]
    
    
    #filter data and model_dists for current combo
    subdat <- data |>
      filter(location == loc,
             target_type == target)
    subdat_dist <- model_dist_data |>
      filter(location == loc,
             target_type == target)
    
    
    
    #go over all forecast dates, get all combinations of models of size nmod 
    #at each date, build ensemble and compute average pairwise historical and 
    #recent distance in ensemble
    
    #init fc_date result list
    fc_date_ensemble_data <- vector(mode = "list",
                                    length = length(fc_dates) - init_weeks)
    
    for(i in (init_weeks+1):length(fc_dates)){
      
      #get current forecast data
      fc_date <- as.Date(fc_dates[i])

          
      #get data subset at fc_date
      fc_date_data <- subdat |>
        filter(forecast_date == fc_dates[i])
      
      
      #get all available models at forecast_date
      #also all combinations of models of size nmod
      avail_models <- unique(fc_date_data$model)
      all_combs <- combn(avail_models, nmod) |> t()
      
      
      #vector of recent dates (to compute recent distance)
      #according to window size
      recent_dates <- seq.Date(
        fc_date - (window * 7), #not window +1
        fc_date - 7,
        by = 7
      )
      
      #####compute distances for all available models#####
      #historical (all past)
      hist_dist_all <- subdat_dist |>
        filter(forecast_date < fc_date) |>
        group_by(model1, model2) |>
        summarise(distavg = mean(dist, na.rm = TRUE)) |>
        mutate(distavg = 
                 ifelse(is.nan(distavg), NA, distavg)) |>
        model_dists_to_mat()
      #recent (only past weeks in current window)
      recent_dist_all <- subdat_dist |>
        filter(forecast_date %in% recent_dates) |>
        group_by(model1, model2) |>
        summarise(distavg = mean(dist, na.rm = TRUE)) |>
        mutate(distavg = 
                 ifelse(is.nan(distavg), NA, distavg)) |>
        model_dists_to_mat()
      
      
      #initialize result container for combo
      #faster than rbind after every iteration in next loop
      comb_ensemble_data <- vector(mode = "list", 
                                   length = nrow(all_combs))
      
      
      ######loop over all model combinations#######
      for(j in 1:nrow(all_combs)){
        
        #current combinations and all pairs within
        ens_comb <- all_combs[j,]
        ens_combs <- combn(ens_comb, 2) |> t()
        
        #get data with only those models 
        ens_dat <- fc_date_data |>
          filter(model %in% ens_comb)
        
        
        
        ########compute distance metrics for ens_comb#####
        #extract all pairwise distances
        hist_dist_mat <- apply(ens_combs, 1, 
                               function(x) hist_dist_all[x[1], x[2]])
        #compute mean and standard deviation
        #if all values are NA, need to specify to return NA
        #rather than NaN
        mean_hist_dist <- 
          ifelse(is.nan(mean(hist_dist_mat, na.rm = TRUE)),
                 NA, mean(hist_dist_mat, na.rm = TRUE))
        sd_hist_dist <- sd(hist_dist_mat, na.rm = TRUE)
        
        #same for recent distance metrics
        recent_dist_mat <- apply(ens_combs, 1,
                                 function(x) recent_dist_all[x[1], x[2]])
        mean_recent_dist <-
          ifelse(is.nan(mean(recent_dist_mat, na.rm = TRUE)),
                 NA, mean(recent_dist_mat, na.rm = TRUE))
        sd_recent_dist <- sd(recent_dist_mat, na.rm = TRUE)
        
        
        ###########build ensembles#########
        ens_dat <- ens_dat |>
          make_ensemble(mean) |>
          make_ensemble(extra_excl = "mean_ensemble") |>
          filter(model %in% c("mean_ensemble",
                              "median_ensemble")) |>
          #add in info about included models and distance measures
          mutate(inc_models = paste(ens_comb, collapse = ";"),
                 nmod = nmod,
                 mean_hist_dist = mean_hist_dist, 
                 sd_hist_dist = sd_hist_dist,
                 mean_recent_dist = mean_recent_dist,
                 sd_recent_dist = sd_recent_dist)
        
        
        #bind to previous results
        comb_ensemble_data[[j]] <- ens_dat
        
      }
      
      all_comb_ensemble_data <- data.table::rbindlist(comb_ensemble_data)
      
      fc_date_ensemble_data[[(i-init_weeks)]] <- 
        all_comb_ensemble_data
      
    }
    
    all_ensemble_data <- rbind(
      all_ensemble_data,
      data.table::rbindlist(fc_date_ensemble_data)
    )
  }
  return(all_ensemble_data)
}


all_combs_ensemble3 <- function(data, 
                               nmod = 3, 
                               window = 5,
                               init_weeks = 5,
                               avail_threshold = 0.5,
                               avail_overlap_threshold = 0.1,
                               excl = c("EuroCOVIDhub-baseline",
                                        "EuroCOVIDhub-ensemble")){
  data <- data |>
    filter(!model %in% excl,
           availability >= avail_threshold)
  
  locs <- unique(data$location)
  targets <- unique(data$target_type)
  
  combos <- paste(rep(locs, each = length(targets)), 
                  targets, sep = ".")
  
  #init result container 
  all_ensemble_data <- NULL #for all ensemble predictions
  all_dist_dt <- NULL #for all pairwise distances
  
  for(comb in combos){
    print(comb)
    
    #get location and target
    filters <- strsplit(comb, split = "[.]")[[1]]
    loc <- filters[1]
    target <- filters[2]
    
    subdat <- data |>
      filter(location == loc,
             target_type == target)
    
    fc_dates <- subdat |>
      select(forecast_date) |>
      distinct() |>
      pull() |>
      sort()
    
    
    #go over all forecast dates, get all combinations of models of size nmod 
    #at each date, build ensemble and compute average pairwise historical and 
    #recent distance in ensemble
    for(i in (init_weeks+1):length(fc_dates)){
      #get current forecast data
      fc_date <- as.Date(fc_dates[i])
      print(fc_date)
      #get data subset at fc_date
      fc_date_data <- subdat |>
        filter(forecast_date == fc_dates[i])
      
      #vector of available models
      avail_models <- unique(fc_date_data$model)
      
      
      #####compute historical distance#####
      hist_dist_all <- subdat |>
        filter(model %in% avail_models,
               forecast_date < as.Date(fc_date)) |>
        model_dist(avail_threshold,
                   avail_overlap_threshold,
                   cramers_dist)
      #result is a list with one element, extract it
      hist_dist_all <- hist_dist_all[[1]]
      hist_dist_dt <- model_dists_to_dt(hist_dist_all) |>
        rename(historical_distance = distance)
      
      
      #####compute recent distance####
      #recent history dates (according to window)
      recent_dates <- seq.Date(
        fc_date - (window * 7), #not window +1
        fc_date - 7,
        by = 7
      )
      #saving some time at initial forecast_date, as historical
      #and recent time frames are the same 
      if(i == (init_weeks+1)){
        recent_dist_all <- hist_dist_all
        recent_dist_dt <- hist_dist_dt |>
          rename(recent_distance = historical_distance)
      } else {
        recent_dist_all <- subdat |>
          filter(model %in% avail_models,
                 forecast_date %in% recent_dates) |>
          model_dist(avail_threshold, 
                     avail_overlap_threshold, 
                     cramers_dist)
        #result is a list with one element, extract it
        recent_dist_all <- recent_dist_all[[1]]
        recent_dist_dt <- model_dists_to_dt(recent_dist_all) |>
          rename(recent_distance = distance)
      }
      
      #make a data.table with all pairwise distances (recent and historical)
      temp_all_dist_dt <- full_join(recent_dist_dt, 
                                    hist_dist_dt,
                                    by = c("model1", "model2")) |>
        mutate(location = loc,
               target_type = target,
               forecast_date = fc_date)
      all_dist_dt <- rbind(all_dist_dt, temp_all_dist_dt)      
      
      
      #update avail_models (sometimes a model is available at fc_date,
      #but not in the history before)
      avail_models <- rownames(hist_dist_all)
      
      
      #make all possible combinations of size nmod
      all_combs <- combn(avail_models, nmod) |> t()
      
      for(j in 1:nrow(all_combs)){
        #current combination
        ens_comb <- all_combs[j,]
        
        #data with only those models 
        ens_dat <- fc_date_data |>
          filter(model %in% ens_comb)
        
        
        #all pairs from ens_comb, to extract from distance matrices
        ens_combs <- combn(ens_comb, 2) |> t()
        
        #compute historical distance metrics
        hist_dist_mat <- apply(ens_combs, 1, 
                               function(x) hist_dist_all[x[1], x[2]])
        mean_hist_dist <- mean(hist_dist_mat, na.rm = TRUE)
        sd_hist_dist <- sd(hist_dist_mat, na.rm = TRUE)
        
        #compute recent distance metrics
        recent_dist_mat <- tryCatch(
          {
            apply(ens_combs, 1,
                  function(x) recent_dist_all[x[1], x[2]])
          },
          error = function(e){ 
            print(paste0(fc_date, ens_comb))
            print(e)
          },
          finally = {})
        
        mean_recent_dist <- mean(recent_dist_mat, na.rm = TRUE)
        sd_recent_dist <- sd(recent_dist_mat, na.rm = TRUE)
        
        
        #build ensembles
        ens_dat <- ens_dat |>
          make_ensemble(mean) |>
          make_ensemble(extra_excl = "mean_ensemble") |>
          filter(model %in% c("mean_ensemble",
                              "median_ensemble")) |>
          #add in info about included models and distance measures
          mutate(inc_models = paste(ens_comb, collapse = ";"),
                 nmod = nmod,
                 mean_hist_dist = mean_hist_dist, 
                 sd_hist_dist = sd_hist_dist,
                 mean_recent_dist = mean_recent_dist,
                 sd_recent_dist = sd_recent_dist)
        
        #bind to previous results
        all_ensemble_data <- all_ensemble_data |>
          rbind(ens_dat)
        
      }
    }
  }
  
  return(list(all_ensemble_data, all_dist_dt))
}