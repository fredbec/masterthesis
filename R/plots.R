#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @description 
#' Plots the results of leaveout_ensemble
#'
#' @param score_data score table output from leaveout_ensemble function
#' @param score which score to evaluate (needs to match column names as 
#'              induced by scoringutils' score() function)
#'              default is interval_score
#' @param givedata should relative score data be returned instead of plot
#' @param title optional, alternative title
#' @param saveplot should plot be saved in pdf format?
#' @param path where to save plot to 
#' 
#' @export

plot_leaveout <- function(score_data, 
                          score = "interval_score", 
                          givedata = FALSE, 
                          title = NULL, 
                          saveplot = TRUE,
                          path = here("plots", "leaveout_ensemble.pdf")
){
  
  #divide all values by those of ensemble using all models
  rel_data <- score_data |>
    dplyr::filter(nmod == "all") |>
    dplyr::select(model, target_type,
                  location, interval_score) |> #dataset of only "all", to merge 
    dplyr::rename(ref = all_of(score)) |>
    merge(score_data, 
          by = c("model", "target_type", "location")) |>
    dplyr::mutate(relative_score = get(score)/ref) |> 
    dplyr::select(model, target_type, location, nmod,
                  relative_score, dplyr::all_of(c(score)))
  
  
  if(givedata){
    return(rel_data)
  }
  
  
  if(is.null(title)){
    title <- "Relative score of ensemble using subset of models"
  }
  
  plot <- rel_data |>
    dplyr::filter(nmod != "all") |> #all obs are one
    dplyr::mutate(nmod = as.numeric(nmod)) |>
    ggplot2::ggplot(aes(x = nmod, y = relative_score,
                        color = model, linetype = target_type)) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ location) + 
    ggplot2::labs(x = "Number of models sampled for ensemble",
                  y = "Score relative to full ensemble",
                  title = title)
  
  print(plot)
  
  if(saveplot){
    ggplot2::ggsave(
      filename = path,
      width = 12, height = 9
    )
  }
  
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import viridis
#' 
#' @description 
#' Plots the results of model_dist as heatmaps
#'
#' @param model_dists output from model_similarity
#' @param which_combs which combinations of location and target_type
#'          should be plotted (required format: GB.Deaths, DE.Cases,...)
#' @param saveplot should plot output be saved
#' @param path where plot should be saves
#' 
#' 
#' @export
#' 

plot_model_similarity <- function(model_dists,
                                  which_combs = NULL,
                                  saveplot = TRUE,
                                  path = here("plots", 
                                              "model_similarity.pdf"),
                                  plotsize = c(width = 12.5, 
                                               height = 9)){
  
  #don't filter if which_combs argument is NULL
  if(is.null(which_combs)){
    which_combs <- names(model_dists)
  }
  
  #make one data.table from all matrices 
  model_dists_dts <- lapply(model_dists, 
                            model_dists_to_dt) |>
    data.table::rbindlist(idcol = TRUE) |>
    dplyr::filter(.id %in% which_combs) |>
    #for plotting, looks nicer (i.e. GB.Deaths to GB - Deaths)
    dplyr::mutate(.id = gsub("\\.", " - ", .id))
  
  
  #make plot
  dist_plot <- ggplot2::ggplot(model_dists_dts,
                               aes(x = model1,
                                   y = model2,
                                   fill = distance)) +
    ggplot2::geom_tile() + 
    #free scale, otherwise all models would be in each heatmap (lots of white)
    ggplot2::facet_wrap(~.id, scales = "free") +
    viridis::scale_fill_viridis(option = "rocket", 
                                direction = -1, 
                                na.value = "white") +
    ggplot2::theme(axis.title.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text = element_text(size=6.5),
                   plot.title = element_text(size = 8),
                   #vertical text
                   axis.text.x = 
                     element_text(angle = 90, vjust = 0.5, hjust=1),
                   )
  
  print(dist_plot)
  
  if(saveplot){
    ggplot2::ggsave(
      filename = path,
      width = plotsize['width'], 
      height = plotsize['height']
    )
  }
  
  
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import gridExtra
#' @description 
#' Plots the results of model_similarity_kickout
#'
#' @param model_kickout_results output from model_similarity_kickout
#' @param random_kickout_results output from kickout_ensemble
#' @param plot_score which scoring function to plot
#' @param saveplot should plot output be saved
#' @param path where plot should be saves
#' @param plotsize size of plotting output
#' 
#' @export
#' 


plot_kickout_results <- function(model_kickout_results,
                                 random_kickout_results,
                                 plot_score = "interval_score",
                                 saveplot = TRUE,
                                 path = here("plots", 
                                             "model_similarity_kickout.pdf"),
                                 plotsize = c(width = 9,
                                              height = 12)){
  
  #prepare random_model_kickout
  #stratify by horizon
  random_kickout_results <- random_kickout_results |>
    dplyr::group_by(nmod, model, target_type, location, horizon) |>
    #get score quantiles of random kickout
    dplyr::summarise(
      p05 = quantile(get(plot_score), probs = c(0.05)),
      p25 = quantile(get(plot_score), probs = c(0.25)),
      p50 = quantile(get(plot_score), probs = c(0.5)),
      p75 = quantile(get(plot_score), probs = c(0.75)),
      p95 = quantile(get(plot_score), probs = c(0.95))
      )

  
  #get results for nmod = 0 
  refvals <- model_kickout |>
    dplyr::filter(nmod == 0) |>
    dplyr::rename(refval = plot_score) |>
    dplyr::select(model, target_type, location, 
                  horizon, refval)

  
  #merge refval and random_kickout into model_kickout results
  joined_dat <- model_kickout_results |> 
    dplyr::filter(nmod > 0) |>
    dplyr::rename(plot_score = plot_score) |>
    dplyr::select(model, target_type, location, 
                  horizon, nmod, plot_score) |>
    dplyr::inner_join(refvals, by = c("model", "target_type", 
                                      "location", "horizon")) |>
    dplyr::inner_join(random_kickout_results,
               by = c("model", "nmod", "target_type", 
                      "location", "horizon")) |>
    dplyr::mutate(nmod = factor(nmod))
  
    
  

  model_sim_plots <- list()
  targets <- unique(joined_dat$target_type)
  locs <- unique(joined_dat$location)
  
  for(loc in locs){
    for(target in targets){
    
      model_sim_plots[[paste0(loc, ".", target)]] <- 
        joined_dat |>
        dplyr::filter(target_type == target, location == loc) |>
        ggplot2::ggplot(
          aes(x = nmod, y = plot_score, group = 1)) +
        ggplot2::geom_line(
          aes(linetype = 'similarity_kickout')) +
        
        #refvalue (horizontal line)
        ggplot2::geom_hline(aes(yintercept = refval, 
                                linetype = 'hub-ensemble')) +
        #ggplot2::geom_line(aes(x = nmod, y = p50), size = 1) +
        
        ###shaded areas for quantiles###
        ggplot2::geom_ribbon(
          aes(x = nmod, ymin = p25, ymax = p75, alpha = 'Q50'), 
          fill = "blue") +
        ggplot2::geom_ribbon(
          aes(x = nmod, ymin = p05, ymax = p95, alpha = 'Q90'), 
          fill = "blue") + 
        ggplot2::geom_ribbon(
          aes(x = nmod, ymin = p50, ymax = p50, alpha = 'Median'), 
          fill = "black") + 
        
        ###make Legends###
        ggplot2::scale_alpha_manual(
          name='Interval',
          breaks=c('Q50', 'Q90', 'Median'),
          values=c('Q50'=0.35, 'Q90'=0.15, 'Median' = 1)) +
        ggplot2::scale_linetype_manual(
          name = 'Type',
          breaks = c('similarity_kickout', 'hub-ensemble'),
          values = c('similarity_kickout' = 'dashed',
                     'hub-ensemble' = 'dotted')) +
        #fix grey empty space at the edges
        ggplot2::scale_x_discrete(limits = factor(c(1,2,3,4)), 
                                  expand = c(0, 0)) +
        ggplot2::facet_wrap(model~horizon, scales = "free", 
                            nrow = 2, ncol = 4) +
        ggplot2::ggtitle(paste0(loc, " - ", target))
        
        
      
    }
  }
  
  pdf(file = path, width = 9, height = 12)
  for(loc in locs){
  gridExtra::grid.arrange(
    model_sim_plots[[paste0(loc, ".Deaths")]], 
    model_sim_plots[[paste0(loc, ".Cases")]],
    nrow = 2, ncol = 1) 
  }
  dev.off()
  
}




#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import viridis 
#'
#' @description 
#' Plots model availability
#'
#' @param data full data (or subset of) from European Forecast Hub
#' @param saveplot should plotting output be saved
#' @param path where plotting output should be saved
#' 
#' @export
#' 
#' 

plot_model_availability <- function(data,
                                    saveplot = TRUE,
                                    path = (here("plots", 
                                                 "availability.pdf"))){
  
  
  plot_model_avail_total <- data |>
      dplyr::group_by(forecast_date, location, target_type) |>
      dplyr::summarise(nmods = length(unique(model))) |>
      dplyr::ungroup() |>
      ggplot2::ggplot(
        aes(x = forecast_date, y = nmods,
            group = location, 
            color = location)) +
      ggplot2::geom_line() + 
      ggplot2::labs(title = "Total Model Availability") +
      ggplot2::facet_grid(~ target_type) +
      ggplot2::ylim(5, 25)
  
  plot_model_avail_indiv <- data |>
      dplyr::select(model, location, 
             target_type, availability) |>
      dplyr::distinct() |>
      ggplot2::ggplot(aes(x = location, y = model,
                          fill = availability)) +
      ggplot2::geom_tile()+
      ggplot2::facet_wrap(~target_type) + 
      viridis::scale_fill_viridis(option = "rocket") +
      ggplot2::theme(
        axis.text.y = element_text(angle = 0, size = 7)) +
      ggplot2::labs(title = "Individual Model Availability")
  
  if(saveplot){
    pdf(file = path)
    print(plot_model_avail_total)
    print(plot_model_avail_indiv)
    dev.off()
  } else {
    print(plot_model_avail_total)
    print(plot_model_avail_indiv)
  }
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import RColorBrewer
#'
#' @description 
#' Plots trajectories of Deaths and Cases
#'
#' @param data full data (or subset of) from European Forecast Hub
#' @param saveplot should plotting output be saved
#' @param path where plotting output should be saved
#' 
#' @export
#' 


plot_trajectories <- function(data,
                              saveplot = TRUE,
                              path = here("plots", "trajectories.pdf")){
 
  trajectories <- data |>
    dplyr::filter(horizon == 1) |>
    dplyr::mutate(
      incidence = 100000 * (true_value / population)) |> #for easier comparison
    dplyr::select(
      location, target_end_date, true_value, 
      target_type, incidence) |>
    dplyr::distinct() |>
    ggplot2::ggplot(
      aes(x = target_end_date, y = incidence,
          group = location, color = location)) +
    ggplot2::geom_line(size = 1.2) + 
    ggplot2::scale_color_brewer(palette = "GnBu") + 
    facet_wrap(~target_type,
               scales = "free") #plots need different scales
  
  if(saveplot){
    pdf(file = path,
        height = 8.3, 
        width = 14.7)
    print(trajectories)
    dev.off()
  } else {
    print(trajectories)
  }

}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#'
#' @description 
#' Plots score results of best performers
#'
#' @param best_performers_scores score results from running 
#'            best_performers_ensemble
#' @param saveplot should plotting output be saved
#' @param path where plotting output should be saved
#' 
#' @export
#' 



plot_best_performers_scores <- 
  function(best_performers_scores,
           saveplot = TRUE,
           path = here("plots","best_performers_scores.pdf")
           ){
  
  
  best_performers_score_plot <- best_performers_scores |>
    dplyr::mutate(best_perf = factor(best_perf,
                                     levels = c(0,1),
                                     labels = c("hub-ens",
                                                "best-perf-ens")),
                  model = factor(model,
                                 levels = c("mean_ensemble",
                                            "median_ensemble"),
                                 labels = c("mean-ens",
                                            "median-ens"))) |>
    dplyr::rename(method = model,
                  strategy = best_perf) |>
    ggplot2::ggplot(aes(x = horizon, y = interval_score)) +
    ggplot2::geom_line(aes(color = method,
                           linetype = strategy)) +
    ggplot2::facet_wrap(location ~ target_type,
                        nrow = 5, ncol = 2,
                        scales = "free") +
    ggtitle("Best performers scores")
  
  
  if(saveplot){
    pdf(path)
    print(best_performers_score_plot)
    dev.off()
    
  } else {
    print(best_performers_score_plot)
  }
}




#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#'
#' @description 
#' Plots model pick results of best_performers
#'
#' @param best_performers_models model picking results from running 
#'            best_performers_ensemble
#' @param data full data (or subset of) from European Forecast Hub
#' @param saveplot should plotting output be saved
#' @param path where plotting output should be saved
#' 
#' @export
#' 
#' 
#' 

plot_best_performers_models <- 
  function(best_performers_models,
           data,
           saveplot = TRUE,
           path = here("plots", "best_performers_models.pdf")){
    
    tables <- list()
    for(comb in names(best_performers_models[2:length(names(best_performers_models))])){
      id <- strsplit(comb, split = "[.]")[[1]]
      
      tables[[comb]] <- best_performers_models[[comb]] |>
        colSums() |>
        t() |> t() |>
        data.table(keep.rownames = TRUE) |>
        mutate(location = id[1],
               target_type = id[2])
      
    }
    
    best_models_data <- rbindlist(tables) |>
      rename(model = rn,
             count = V1)
    
    model_types <- data |>
      mutate(model_type = replace(model_type, 
                                  model_type=="ensemble", "other"))|>
      select(model, model_type) |>
      distinct()

    
    best_models_plot <- best_models_data |>
      dplyr::inner_join(model_types, by = c("model")) |>
      dplyr::group_by(location, target_type) |>
      dplyr::arrange(count, .by_group = TRUE) |>
      dplyr::mutate(model = replace(model, count < 12, "other")) |>
      ggplot2::ggplot(aes(x = location, y = count, fill = model)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::facet_wrap(~target_type) +
      ggplot2::ggtitle("Chosen models")
      #ggplot2::scale_fill_manual(
      #  values = diverge_hsv(length(unique(best_models_data$model))))
    
    model_type_plot <- best_models_data |>
      dplyr::inner_join(model_types, by = c("model")) |>
      ggplot2::ggplot(aes(x = location, y = count, fill = model_type)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::facet_wrap(~target_type) +
      ggplot2::ggtitle("Model types chosen models")
    
    model_type_compare_plot <- data |>
      dplyr::select(model, location, target_type, model_type) |>
      dplyr::filter(model_type != "ensemble") |>
      dplyr::distinct() |>
      dplyr::mutate(count = 1) |>
      ggplot2::ggplot(aes(x = location, fill = model_type)) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::facet_wrap(~target_type) +
      ggplot2::ggtitle("Model types overall")

    
    if(saveplot){
      pdf(file = path, width = 12, height = 9)
      
      gridExtra::grid.arrange(
        best_models_plot,
        model_type_plot,
        model_type_compare_plot,
        layout_matrix = rbind(c(1, 1),
                              c(2, 3))) 
      
      dev.off()
    } else {
      print(best_models_plot)
      print(model_type_plot)
      print(model_type_compare_plot)
    }
    
    
  }