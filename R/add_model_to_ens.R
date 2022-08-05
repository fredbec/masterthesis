add_model_to_ens <- function(all_combs_dat, hub_data, loc, window = 5, sample_nmod = 50, seed = 41){
  
  set.seed(seed)
  
  #need this to exclude horizons that are not resolved 
  #yet at current fc_date
  map_fc_to_tg <- hub_data |>
    select(forecast_date, horizon, target_end_date)
  
  #score hub_data (move to external)
  all_scores <- hub_data |>
    filter(location == loc) |>
    score() |>
    summarise_scores(by = c("model",
                            "forecast_date",
                            "target_type",
                            "horizon")) |> 
    left_join(map_fc_to_tg, by = c("forecast_date", "horizon"))
  
  
  #get targets and forecast dates in data
  targets <- unique(all_combs_dat$target_type)
  fc_dates <- unique(all_combs_dat$forecast_date)
  
  
  #overall result containers
  all_target_ensembles <- vector(mode = "list", length = 2)
  names(all_target_ensembles) <- targets
  all_target_model_scores <- vector(mode = "list", length = 2)
  names(all_target_model_scores) <- targets
  
  for(target in c("Deaths")){
    
    #result containers for forecast_date loop level
    all_fc_date_ensembles <- vector(mode = "list", length = length(fc_dates))
    all_fc_date_model_scores <- vector(mode = "list", length = length(fc_dates))
    
    #for (i in 1:length(fc_dates)){
      
    for (i in 1:3){
      #current forecast data
      fc_date <- fc_dates[i]
      print(fc_date)
      
      #get window of recent dates to judge worst/best model
      recent_dates <- fc_dates_window(fc_date, 4)
      
      #get relevant subset of all_combs
      subdat_all_combs <- all_combs_dat |>
        filter(forecast_date == fc_date,
               target_type == target)
      
      #get relevant subset of scores
      #filter such that only resolved horizons enter
      subdat_scores <- all_scores |>
        filter(forecast_date %in% recent_dates,
               target_type == target,
               target_end_date < fc_date) 
      
      #get relevant subset of hub_data
      subdat_hub <- hub_data |>
        filter(forecast_date == fc_date,
               location == loc,
               target_type == target)
      
      #get list of ensembles to loop over
      ens_models_list <- subdat_all_combs |>
        select(inc_models) |>
        distinct() |>
        sample_n(sample_nmod) |>
        pull()
    
      
      #remove baseline and ensemble from model list 
      #as these should never be added to an ensemble
      all_models <- setdiff(unique(subdat_hub$model), 
                            c("EuroCOVIDhub-baseline",
                              "EuroCOVIDhub-ensemble"))
      
      #get recent list of available models
      all_models_recent <- setdiff(unique(subdat_scores$model),
                                   c("EuroCOVIDhub-baseline",
                                     "EuroCOVIDhub-ensemble"))
      
      #result containers to loop over ensembles
      all_ensembles <- vector(mode = "list", length = sample_nmod)
      all_model_scores <- vector(mode = "list", length = sample_nmod)
      
      #loop over ensemble sets
      for(k in 1:length(ens_models_list)){
        #get prediction of current ensemble (mean/median)
        ens_models_pred <- subdat_all_combs |>
          filter(inc_models == ens_models_list[k])
        
        #get actual list of models from concatenated string
        ens_models <- strsplit(ens_models_list[k], ";")[[1]]
        
        
        #sample models to propose (ofc exclude those already in ensemble)
        prop_mods <- setdiff(all_models, ens_models) |> 
          sample(3)
        
        prop_mods_recent <- setdiff(all_models_recent, ens_models) |>
          intersect(all_models) |> #only propose models that are currently available
          sample(3)
    
        
        #scores of prop_mods, get worst and best models
        scores_prop_mods <- subdat_scores |>
          filter(model %in% prop_mods_recent) |>
          summarise_scores(by = "model") |>
          filter(interval_score %in% c(min(interval_score),
                                       max(interval_score))) |>
          #add some columns for merging with other score data later
          mutate(hist = 1,
                 horizon = NA,
                 ens_type = NA,
                 type = c("worst_mod", "best_mod"))
        
        #get best and worst model
        best_mod <- scores_prop_mods |>
          filter(interval_score == min(interval_score)) |>
          select(model) |>
          pull()
        worst_mod <- scores_prop_mods |>
          filter(interval_score == max(interval_score)) |>
          select(model) |>
          pull()
        
        
        #get relevant column names for rbinding dataset
        cols <- intersect(names(subdat_hub), names(ens_models_pred))
        
        ######most/least distant model 
        ens_models_pred <- ens_models_pred |>
          select(cols) 
        
        ens_models_scores <- ens_models_pred |>
          score() |>
          summarise_scores(by = c("model", "horizon")) |>
          mutate(ens_type = NA,
                type = NA,
                hist = NA)
        
        ens_models_pred <- ens_models_pred |>
          rbind(subdat_hub |> 
                  filter(model %in% prop_mods) |> 
                  select(cols)) |>
          mutate(availability = 1, #this is just so model_dist fct works
                 location = loc,
                 target_type = target)
        
        
        
        #calculate distances
        dists <- ens_models_pred |>
          model_dist(avail_threshold = 1,
                     avail_overlap_threshold = 0,
                     dist_fun = cramers_dist)
        
        #only interested in distance to mean/med ensemble
        dists <- dists[[1]][c(1,2),-c(1,2)]
        
        #get models with max/min distance
        mod_names <- colnames(dists)
        
        min_models <- apply(dists, 1, function(mod) 
          mod_names[which(mod == min(mod, na.rm = TRUE))])
        max_models <- apply(dists, 1, function(mod) 
          mod_names[which(mod == max(mod, na.rm = TRUE))])
        
        #check for multiple minima/maxima
        if(!is.null(nrow(min_models))){
          min_models <- min_models[1,]
        }
        
        if(!is.null(nrow(max_models))){
          max_models <- max_models[1,]
        }
        #this is for the scoring data.table
        names_idx_mods <- c(min_models, max_models) |> names()
        
        idx_mods_dist <- tryCatch(expr = {
          idx_mods_dist <- c(min_models, max_models) |>
            data.table(ens_type = names_idx_mods,
                       type = rep(c("min_dist", "max_dist"), each = 2)) |>
            rename(model = V1)
        }, warning = function(w){
          print("warning1")
          print(idx_mods_dist)
          return("warning1")
          #print(w)
        }
        )
        
        if(length(idx_mods_dist) == 1){
          return(list(dists, min_models, max_models))
        }
        
        tryCatch(expr = {
        idx_mods_perform <- c(best_mod, worst_mod) |>
          data.table(ens_type = NA,
                     type = c("best_mod", "worst_mod")) |>
          rename(model = V1)
        }, warning = function(w){
          print("warning2")
          print(idx_mods_perform)
          print(c(best_mod, worst_mod))
          
        }
        )
        
        idx_mods <- rbind(idx_mods_dist, idx_mods_perform)
        
        scores_mods <- subdat_hub |>
          filter(model %in% unique(idx_mods$model)) |>
          score() |>
          summarise_scores(by = c("model", "horizon")) |>
          mutate(hist = 0) 
        
        
        all_model_scores[[k]] <- inner_join(idx_mods, scores_mods,
                                  by = c("model")) |> #not in pipe bc of ordering
          rbind(scores_prop_mods) |>
          rbind(ens_models_scores) |>
          mutate(idx = k)
        
        
        min_ens_models <- sapply(min_models, 
                                 function(mod) c(ens_models, mod))
        max_ens_models <- sapply(max_models, 
                                 function(mod) c(ens_models, mod))
        worst_ens_models <- c(ens_models, worst_mod)
        best_ens_models <- c(ens_models, best_mod)
        
        #small helper function to make ensembles
        make_ens <- function(ens_models, subdat_hub, which_ens, type, idx){
          subdat <- subdat_hub |>
            filter(model %in% ens_models) |>
            make_ensemble_minimal(which_ens = which_ens) |>
            mutate(type = type,
                   idx = idx)
          return(subdat)
        }
        
        tryCatch(expr = {
        min_ensembles <- lapply(colnames(min_ens_models), function(which_ens)
          make_ens(min_ens_models[,which_ens], subdat_hub, which_ens, type = "min_dist", idx = k)) 
        
        max_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(max_ens_models[,which_ens], subdat_hub, which_ens, type = "max_dist", idx = k))
        
        worst_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(worst_ens_models, subdat_hub, which_ens, type = "worst_mod", idx = k))
        best_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(best_ens_models, subdat_hub, which_ens, type = "worst_mod", idx = k))
        }, error = function(e){
          print(list(min_ens_models, max_ens_models))
        }
        )
        
        all_ensembles[[k]] <- c(min_ensembles, max_ensembles, worst_ensembles, best_ensembles) |>
          rbindlist()
        
        
        #for best/worst: record if it's currently over-or underpredictor/if ensemble is over- or underpredictor
      }
      all_fc_date_ensembles[[i]] <- rbindlist(all_ensembles)
      all_fc_date_model_scores[[i]] <- rbindlist(all_model_scores)
    }
    
    all_target_ensembles[[target]] <- rbindlist(all_fc_date_ensembles)
    all_target_model_scores[[target]] <- rbindlist(all_fc_date_model_scores)
  }
  
  return(list(rbindlist(all_target_ensembles),
         rbindlist(all_target_model_scores)))
}
