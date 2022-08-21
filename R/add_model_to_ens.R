add_model_to_ens <- function(all_combs_dat, hub_data, loc, 
                             nmod,
                             all_scores = NULL,
                             su_cols = specs$su_cols,
                             fc_dates = NULL, window = 5, 
                             sample_nmod = 50, seed = 41,
                             prop_nmod = 3){
  
  set.seed(seed)
  
  #need this to exclude horizons that are not resolved 
  #yet at current fc_date
  map_fc_to_tg <- hub_data |>
    select(forecast_date, horizon, target_end_date)
  
  #score hub_data (could move this to external, but relatively 
  #doesn't really take up that much time)
  if(is.null(all_scores)){
    all_scores <- hub_data |>
      select(su_cols) |>
      filter(location == loc) |>
      score() |>
      summarise_scores(by = c("model",
                              "forecast_date",
                              "target_type",
                              "horizon")) |> 
      left_join(map_fc_to_tg, by = c("forecast_date", "horizon"))
  }
  
  
  
  #get targets and forecast dates in data
  targets <- unique(all_combs_dat$target_type)
  if(is.null(fc_dates)){
    fc_dates <- unique(all_combs_dat$forecast_date)
  }
  
  
  #overall result containers
  all_target_ensembles <- vector(mode = "list", length = 2)
  names(all_target_ensembles) <- targets
  all_target_model_scores <- vector(mode = "list", length = 2)
  names(all_target_model_scores) <- targets
  
  #catchlist_target <- vector(mode = "list", length = 2)
  #names(catchlist_target) <- targets
  
  
  for(target in targets){
    
    #result containers for forecast_date loop level
    all_fc_date_ensembles <- vector(mode = "list", length = length(fc_dates))
    all_fc_date_model_scores <- vector(mode = "list", length = length(fc_dates))
    #catchlist <- vector(mode = "list", length = length(fc_dates))
    
    #print(fc_dates)
    for (i in 1:length(fc_dates)){
    
    #for (i in 1:2){
      #current forecast data
      fc_date <- fc_dates[i]
      print(fc_date)
      
      #get window of recent dates to judge worst/best model
      recent_dates <- fc_dates_window(fc_date, window)
      
      #get relevant subset of all_combs
      subdat_all_combs <- all_combs_dat |>
        filter(forecast_date == fc_date,
               target_type == target)
      
      #get relevant subset of scores
      #filter such that only resolved horizons enter
      subdat_scores <- all_scores |>
        filter(forecast_date %in% recent_dates, 
               target_type == target,
               target_end_date < fc_date) |> #remove as yet unresolved horizons 
        summarise_scores(by = c("model"),
                         relative_skill = TRUE,
                         baseline = "EuroCOVIDhub-baseline")
      if(FALSE){
      subdat_scores <- tryCatch(
        expr = {
          all_scores |>
            filter(forecast_date %in% recent_dates, 
                   target_type == target,
                   target_end_date < fc_date) |> #remove as yet unresolved horizons 
            summarise_scores(by = c("model"),
                             relative_skill = TRUE,
                             baseline = "EuroCOVIDhub-baseline")
        },
        warning = function(w){
          print(w)
          catchlistt <- all_scores |>
            filter(forecast_date %in% recent_dates, 
                   target_type == target,
                   target_end_date < fc_date)
          retuuu <- all_scores |>
            filter(forecast_date %in% recent_dates, 
                   target_type == target,
                   target_end_date < fc_date) |> #remove as yet unresolved horizons 
            summarise_scores(by = c("model"),
                             relative_skill = TRUE,
                             baseline = "EuroCOVIDhub-baseline")
          return(list(retuuu, catchlistt))
        }
      ) 
      if(length(subdat_scores)==2){
        print("hey")
        catchlist[[i]] <- subdat_scores[[2]]
        subdat_scores <- subdat_scores[[1]]
      }
      }
        
      #get relevant subset of hub_data
      subdat_hub <- hub_data |>
        filter(forecast_date == fc_date,
               location == loc,
               target_type == target)
      
      
      #get list of ensembles to loop over
      #(sample from available ensembles)
      ens_models_list <- subdat_all_combs |>
        select(inc_models) |>
        distinct()
      
      if(nrow(ens_models_list) < sample_nmod/2){
        print(fc_date)
        next
      } else if (nrow(ens_models_list) < sample_nmod){
        ens_models_list <- ens_models_list |>
          sample_n(sample_nmod,
                   replace = TRUE) |>
          pull()
      } else {
        ens_models_list <- ens_models_list |>
          sample_n(sample_nmod) |>
          pull()
      }

      #remove baseline and ensemble from model list 
      #as these should never be added to an ensemble
      all_models <- setdiff(unique(subdat_hub$model), 
                            c("EuroCOVIDhub-baseline",
                              "EuroCOVIDhub-ensemble"))
      
      if(length(all_models) < nmod + prop_nmod){
        next
      }
      
      #get recent list of available models
      all_models_recent <- setdiff(unique(subdat_scores$model),
                                   c("EuroCOVIDhub-baseline",
                                     "EuroCOVIDhub-ensemble"))
      
      #result containers to loop over ensembles
      all_ensembles <- vector(mode = "list", length = sample_nmod)
      all_model_scores <- vector(mode = "list", length = sample_nmod)
      
      #loop over ensemble sets
      #for each ensemble set, get models that are currently *not* in the ensemble
      #propose 3 and choose out of those three according to distance/performance
      for(k in 1:length(ens_models_list)){
        #get prediction of current ensemble (mean/median)
        ens_models_pred <- subdat_all_combs |>
          filter(inc_models == ens_models_list[k])
        

        #get actual list of ensemble models from concatenated string
        ens_models <- strsplit(ens_models_list[k], ";")[[1]]
        
        #sample models to propose (exclude those already in ensemble)
        prop_mods <- setdiff(all_models, ens_models) |> 
          sample(prop_nmod)
        
        prop_mods_recent <- setdiff(all_models_recent, ens_models) |>
          intersect(all_models) |> #only propose models that are also currently available
          sample(prop_nmod)
        
        #print(prop_mods_recent)
        
        #scores of prop_mods, get worst and best models
        scores_prop_mods <- subdat_scores |>
          filter(model %in% prop_mods_recent) #|>
          #summarise_scores(by = "model") #|> #|>
        
        
        scores_prop_mods <- scores_prop_mods |>
          filter(scaled_rel_skill %in% c(min(scaled_rel_skill, na.rm = TRUE),
                                       max(scaled_rel_skill, na.rm = TRUE))) |>
          arrange(scaled_rel_skill)
        
        if(nrow(scores_prop_mods)>2){
          print(scores_prop_mods)
          scores_prop_mods <- scores_prop_mods[c(1, nrow(scores_prop_mods)),]
        }
        
        scores_prop_mods <- scores_prop_mods |>
          #add some columns for merging with other score data later
          mutate(hist = 1,
                 horizon = NA,
                 ens_type = NA,
                 type = c("best_mod", "worst_mod"))

        #get best and worst model
        best_mod <- scores_prop_mods[1,1] 
        worst_mod <-  scores_prop_mods[2,1]
        
        #random model
        random_mod <- setdiff(all_models, ens_models) |> 
          sample(1)
        
        #get relevant column names for rbinding dataset
        cols <- intersect(names(subdat_hub), names(ens_models_pred))
        
        #return(cols)
        ####################most/least distant model ###################
        ens_models_pred <- ens_models_pred |>
          select(cols) 
        
        #score mean and median ensemble
        ens_models_scores <- ens_models_pred |>
          select(intersect(su_cols, cols)) |>
          score() |>
          summarise_scores(by = c("model", "horizon")) |>
          mutate(ens_type = NA,
                type = NA,
                hist = NA)
        

        #bind proposal models to data to assess distance
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
        #print(dists)
        
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
        
        #catch case of only one ens type giving two equally distant models -> 
        #min_models is list
        if(is.list(min_models)){
          min_models <- sapply(min_models, function(mods) mods[1])
        } 
        if(is.list(max_models)){
          max_models <- sapply(max_models, function(mods) mods[1])
        } 

        
        #this is for the scoring data.table
        #transform data for which models were chosen into data.tables
        names_idx_mods <- c(min_models, max_models) |> names()
        idx_mods_dist <- c(min_models, max_models) |>
          data.table(type = rep(c("min_dist", "max_dist"), each = 2),
                     ens_type = names_idx_mods) |>
          rename(model = V1)
        idx_mods_perform <- scores_prop_mods |>
          select(model, type) |>
          mutate(ens_type = NA) 
        
        idx_mods <- rbind(idx_mods_dist, idx_mods_perform)
        
        
        scores_mods <- subdat_hub |>
          select(su_cols)|>
          filter(model %in% unique(idx_mods$model)) |>
          score() |>
          summarise_scores(by = c("model", "horizon")) |>
          mutate(hist = 0) 
        ########################ABOVE: ADD RELATIVE SKILL####################?
        
        all_model_scores[[k]] <- tryCatch(expr = {
        inner_join(idx_mods, scores_mods,
                   by = c("model")) |> #not in pipe bc of ordering
          rbind(scores_prop_mods, fill = TRUE) |>
          rbind(ens_models_scores, fill = TRUE) |>
          mutate(idx = k)
        }, error = function(e){
          print(e)
          return("well")
        }
        )
        if(length(all_model_scores[[k]])==1){
          return(list(idx_mods, scores_mods, names_idx_mods, min_models, max_models, dists))
        }
        min_ens_models <- sapply(min_models, 
                                 function(mod) c(ens_models, mod))
        max_ens_models <- sapply(max_models, 
                                 function(mod) c(ens_models, mod))
        worst_ens_models <- c(ens_models, worst_mod)
        best_ens_models <- c(ens_models, best_mod)
        random_models <- c(ens_models, random_mod)


        #small helper function to make ensembles
        make_ens <- function(ens_models, subdat_hub, which_ens, type){
          subdat <- subdat_hub |>
            filter(model %in% ens_models) |>
            make_ensemble_minimal(which_ens = which_ens) |>
            mutate(type = type)
          return(subdat)
        }
        #print(min_ens_models)
        mydat <- subdat_hub |>
          filter(model %in% min_ens_models[,1])
        #make all ensembles based on chosen models
        min_ensembles <- lapply(colnames(min_ens_models), function(which_ens)
          make_ens(min_ens_models[,which_ens], subdat_hub, which_ens, type = "min_dist")) 
        max_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(max_ens_models[,which_ens], subdat_hub, which_ens, type = "max_dist"))
        worst_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(worst_ens_models, subdat_hub, which_ens, type = "worst_mod"))
        best_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(best_ens_models, subdat_hub, which_ens, type = "best_mod"))
        random_ensembles <- lapply(colnames(max_ens_models), function(which_ens)
          make_ens(random_models, subdat_hub, which_ens, type = "random_mod"))
        
        all_ensembles[[k]] <- c(min_ensembles, max_ensembles, worst_ensembles, best_ensembles, random_ensembles) |>
          rbindlist() |>
          mutate(idx = k)
        
        all_ens_scores <- all_ensembles[[k]] |>
          select(-idx) |> #remove redundant col before scoring
          score() |>
          summarise_scores(by = c("model", "type", "horizon")) |>
          mutate(idx = k,
                 hist = 0)
         
        
        #append ensemble scores to all model scores
        all_model_scores[[k]] <- rbind(all_model_scores[[k]], 
                                       all_ens_scores, fill = TRUE)
        
        #for best/worst: record if it's currently over-or underpredictor/if ensemble is over- or underpredictor
      }
      all_fc_date_ensembles[[i]] <- rbindlist(all_ensembles)
      all_fc_date_model_scores[[i]] <- rbindlist(all_model_scores)
    }
    
    all_target_ensembles[[target]] <- rbindlist(all_fc_date_ensembles)
    all_target_model_scores[[target]] <- rbindlist(all_fc_date_model_scores)
    #catchlist_target[[target]] <- rbindlist(catchlist)
  }
  
  return(list(rbindlist(all_target_ensembles),
         rbindlist(all_target_model_scores)))
         #rbindlist(catchlist_target)))
}
