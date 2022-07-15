#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' @import dplyr
#' @import scoringutils
#' 
#' @description 
#' Samples ALL combinations of possible ensembles of size n at given timepoint,
#' scores them and computes average pairwise historical and recent distance
#' 
#' 
#'
#' @param data data (subset or full) from the European Forecast hub
#' @param model_dist_data model distance data (output from model_dist_by_fc_date)
#' @param avail_threshold minimum availability for models to be considered in the
#'        sampling process 
#' @param nmod number of models to sample at each date
#' @param window window size for recent distance computation
#' @param init_weeks number of weeks left out at beginning for init
#' @param excl which models should be excluded from the ensemble experiment

#' @export


all_combs_ensemble <- function(data,
                               model_dist_data,
                               avail_threshold,
                               nmod, 
                               window = 5,
                               init_weeks = 5,
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
      print(fc_date)

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
      
      #####compute distances for all  models#####
      #historical (all past)
      #first, split dataframe by horizon
      hist_dist_all <- subdat_dist |>
        filter(forecast_date < fc_date) |>
        data.table() |>
        split(by = "horizon") 
      #then take average over all fc_dates for each pair and convert
      #to matrix (much faster extraction in next steps)
      hist_dist_all <- lapply(hist_dist_all, function(dat)
        dat |>
          group_by(model1, model2) |>
          summarise(avgdist = mean(dist, na.rm = TRUE)) |>
          mutate(avgdist = 
                   ifelse(is.nan(avgdist), NA, avgdist)) |>
          model_dists_to_mat() #from utils
        )
      
      
      #recent (only past weeks in current window)
      #everything else same as before
      recent_dist_all <- subdat_dist |>
        filter(forecast_date %in% recent_dates)  |> #only difference to above
        data.table() |>
        split(by = "horizon") 
      recent_dist_all <- lapply(recent_dist_all, function(dat)
        dat |>
        group_by(model1, model2) |>
        summarise(avgdist = mean(dist, na.rm = TRUE)) |>
        mutate(avgdist = 
                 ifelse(is.nan(avgdist), NA, avgdist)) |>
        model_dists_to_mat()
      )
      
      
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
        #extract all pairwise distances, by horizon
        hist_dist_mat <- lapply(hist_dist_all, function(mat)
          apply(ens_combs, 1,  #extracts pairwise distances
                function(x) mat[x[1], x[2]])
          ) |> 
          sapply(function(x) #this returns a df-esque object with summary statistics
            c(hist_dist_mean = ifelse(is.nan(mean(x, na.rm = TRUE)), #if all NA, also return NA and not NaN
                                      NA, mean(x, na.rm = TRUE)),
              hist_dist_sd = sd(x),
              hist_pairs_avail = nmod - sum(is.na(x)))) |> #how many pairwise dists are available
          t() |>
          data.table() |>
          mutate(horizon = 1:4)
        
        #same as above
        recent_dist_mat <- lapply(recent_dist_all, function(mat)
          apply(ens_combs, 1, 
                function(x) mat[x[1], x[2]])
          ) |>
          sapply(function(x) 
            c(recent_dist_mean = ifelse(is.nan(mean(x, na.rm = TRUE)), #if all NA, also return NA and not NaN
                                        NA, mean(x, na.rm = TRUE)),
              recent_dist_sd = sd(x),
              recent_pairs_avail = nmod - sum(is.na(x)))) |> #how many pairwise dists are available
          t() |>
          data.table() |>
          mutate(horizon = 1:4)
        
        
        
        ###########build ensembles#########
        ens_dat <- ens_dat |>
          make_ensemble(mean) |>
          make_ensemble(extra_excl = "mean_ensemble") |>
          filter(model %in% c("mean_ensemble",
                              "median_ensemble")) |>
          #add in info about included models and distance measures
          mutate(inc_models = paste(ens_comb, collapse = ";"),
                 nmod = nmod) |>
          #join with distance dataframes
          left_join(hist_dist_mat,
                    by = c("horizon")) |>
          left_join(recent_dist_mat,
                    by = c("horizon"))
        
        
        #put in storage object
        comb_ensemble_data[[j]] <- ens_dat
        
      }
      
      #bind together all comb_ensemble_data, then put in storage object
      all_comb_ensemble_data <- 
        data.table::rbindlist(comb_ensemble_data)
      fc_date_ensemble_data[[(i-init_weeks)]] <- 
        all_comb_ensemble_data
    
    }
    
    #bind together
    #could have also done storage as before, but since this only happens
    #a few times, speedup is probably negligible 
    all_ensemble_data <- rbind(
      all_ensemble_data,
      data.table::rbindlist(fc_date_ensemble_data)
    )
  }
  return(all_ensemble_data)
}
