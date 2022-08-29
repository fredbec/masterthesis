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
#' @import RColorBrewer
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
                                                 "availability.pdf")),
                                    height = 12, 
                                    width = 8,
                                    indiv = TRUE,
                                    main = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    palette = "Set1"){
  
  
  plot_model_avail_total <- data |>
      dplyr::group_by(forecast_date, location, target_type) |>
      dplyr::summarise(nmods = length(unique(model))-2) |> #subtract 2 bc of baseline, ensemble
      dplyr::ungroup() |>
      ggplot2::ggplot(
        aes(x = forecast_date, y = nmods,
            group = location, 
            color = location)) +
      ggplot2::geom_line() + 
      ggplot2::labs(title = main) +
      ggplot2::facet_grid(~ target_type) +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::ylim(5, 25)+
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)  
  
  plot_model_avail_indiv <- data |>
    dplyr::select(model, location, 
           target_type, availability) |>
    dplyr::distinct() |>
    ggplot2::ggplot(aes(x = model, y = location,
                        fill = availability)) +
    ggplot2::geom_tile()+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                   axis.text.y = element_text(size = 12),
                   plot.margin = margin(5, 5, 5, 15),
                   legend.position = "right") +
    ggplot2::scale_y_discrete(name ="", 
                              breaks = c("PL", "GB", "FR", "DE", "CZ"),
                              labels=c("Poland","Great Britain","France",
                                       "Germany", "Czech R.")) +
    ggplot2::facet_wrap(~target_type) + 
    viridis::scale_fill_viridis(option = "rocket",
                                name = "") +
    ggplot2::theme(
      axis.text.y = element_text(angle = 0, size = 7)) +
    ggplot2::labs(title = main) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  if(saveplot){
    pdf(file = path, height= height, width = width)
    if(indiv){
      print(plot_model_avail_indiv)
    } else {
      print(plot_model_avail_total)
    }
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
                              fc_dates_split = NULL,
                              saveplot = TRUE,
                              path = here("plots", "trajectories.pdf"),
                              height = 7,
                              width = 9.5
                              ){
  
  if(is.null(fc_dates_split)){
    fc_dates <- sort(unique(data$forecast_date))
    fc_dates <- c(fc_dates, fc_dates[length(fc_dates)] + 7)
    splitter <- c(1,10,10,9,9,9) |> cumsum()

    date_list <- lapply(1:5, function(i) 
      c(fc_dates[splitter[i]], 
        fc_dates[(splitter[i+1])]))
    
  }
  #return(date_list)

  trajectories <- data |>
    dplyr::filter(horizon == 1) |>
    dplyr::mutate(
      incidence = ifelse(target_type == "Deaths",
                         true_value,
        100000 * (true_value / population))) |> #for easier comparison
    dplyr::select(
      location, target_end_date, true_value, 
      target_type, incidence) |>
    dplyr::distinct() |>
    ggplot2::ggplot(
      aes(x = target_end_date, y = incidence,
          group = location, color = location)) +
    ggplot2::geom_line() +
    labs(y = "Incidence", x = "", legend = "") +
    theme_masterthesis() + 
    ggplot2::geom_point(shape = 18) +
    ggplot2::scale_color_brewer(palette = "Set2",
                                breaks = names(specs$plot_location_label),
                                labels = unname(specs$plot_location_label),
                                name = "") + 
    lapply(seq_along(date_list), function(dt) {
      list(annotate("rect", xmin = as.IDate(date_list[[dt]][1]), 
                    xmax = as.IDate(date_list[[dt]][2]), ymin = 0, ymax = Inf, 
                    alpha = .15 * (dt%%2 + 1)))
    }
      )+
    #geom_vline(xintercept=as.IDate("2021-03-15"),linetype="dotted",size=0.6)+
    #geom_vline(xintercept=as.IDate("2021-05-17"),linetype="dotted",size=0.6) +
    #geom_rect(aes(xmin=as.IDate("2021-03-15"), 
    #              xmax=as.IDate("2021-05-17"), ymin=0, ymax=Inf),
    #          alpha = 0.3)+
    #ggplot2::scale_color_discrete(labels = labeller(specs$plot_location_label)) + 
    facet_wrap(~target_type, nrow = 2,
               labeller = as_labeller(specs$plot_target_label),
               scales = "free") #plots need different scales
  
  if(saveplot){
    pdf(file = path,
        height = height, 
        width = width)
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

#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import gridExtra
#' @description 
#' Plots model_distance vs performance
#' @export
#Note: data will be stratified by forecast_date, horizon, model, target_type
#this also means: different locations will be averaged over. If this is not 
#desired, filter data beforehand

plot_dist_vs_scores_overtime <- function(
    plot_data, 
    score_data = NULL,
    palette = "Set2",
    min_x.ticks = TRUE){
  
  #if(length(unique(plot_data$target_type))>1 | length(unique(score_data$target_type))>1){
  #  warning("do you really want to average over target types?")
  #}
  
  #make boxplots
  boxplot_data <- plot_data |> 
    select(forecast_date, inc_models, horizon, target_type, 
           hist_dist_mean, recent_dist_mean, current_dist_mean) |>
    distinct() 
  
  
  #this is some stupid code for the tick marks
  #it's stupid for two reasons: a. it's ugly, b. it doesn't even work
  dates <- c("2021-07-05","2021-10-04","2021-12-06")
  labs <- c()
  prev_loc <- 0
  for(dat in dates){
    loc <- which(sort(unique(plot_data$forecast_date))==dat)
    labs <- c(labs, rep("", times = (loc-prev_loc)-1), dat)
    prev_loc <- loc
  }
  loc <- length(unique(plot_data$forecast_date)) - loc
  labs <- c(labs, rep("", times = loc))
  
  
  box_plot_dist <- ggplot(boxplot_data, 
                          aes(x = factor(sort(forecast_date)), 
                              y = current_dist_mean, 
                              fill = factor(horizon))) +
    geom_boxplot(outlier.shape = 20) +
    scale_fill_brewer(palette = palette) +
    scale_x_discrete(breaks=sort(unique(plot_data$forecast_date)),
                     labels=labs) +
    xlab("") +
    ylab("Mean ensemble distance") +
    labs(fill='Horizon') +
    scale_y_continuous(trans = "log10", labels = scales::comma)
  
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  #facet_wrap(~target_type, ncol = 2, scales = "free")
  
  
  #this step takes a while, so best to supply from outside
  if(is.null(score_data)){
    
    score_data <- plot_data |>
      score() |>
      summarise_scores(by = c("forecast_date", "horizon", "model", "target_type"))
  }
  
  score_data <- score_data |>
    filter(model == "median_ensemble")
  
  line_plot_perform <- ggplot(score_data, aes(x = (sort(forecast_date)), 
                                              y = interval_score,
                                              color = factor(horizon))) +
    #shape = factor(model)))) +
    #shape = factor(model))) +
    geom_line() +
    scale_color_brewer(palette = palette) +
    xlab("Forecast Date") +
    ylab("Interval Score") +
    labs(color='Horizon') +
    scale_y_continuous(trans = "log10", labels = scales::comma)
  #facet_wrap(~target_type, ncol = 2, scales = "free")
  
  #arrange in single plot
  plots_dist <- gridExtra::grid.arrange(box_plot_dist, line_plot_perform, nrow = 2)
  
  return(plots_dist)
}




#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#' @import gridExtra
#' @description 
#' Plots model_distance vs performance
#NEEDS WORK, STILL VERY BASIC
basic_dist_ovr_hor <- function(plot_dat,
                               quantiles = seq(0.1, 0.9, 0.1)){
  
  
  plot_dat_quants <- plot_dat |> 
    select(forecast_date, inc_models, horizon, target_type, 
           hist_dist_mean, recent_dist_mean, current_dist_mean) |>
    distinct() |> 
    group_by(horizon, target_type) |>
    summarise(quantile = paste0("quant", quantiles),
              myval = quantile(current_dist_mean,
                               quantiles)) |>
    setDT() |>
    dcast(horizon + target_type ~ quantile)
  
  
  spread_plot <- ggplot(czdat_min, aes(x = horizon, y = quant0.5)) +
    geom_line() +
    ggplot2::geom_ribbon(
      aes(x = horizon, ymin = quant0.2, ymax = quant0.8, alpha = 'Q50'), 
      fill = "blue") + 
    ggplot2::geom_ribbon(
      aes(x = horizon, ymin = quant0.1, ymax = quant0.9, alpha = 'Q50'), 
      fill = "blue") + 
    ggplot2::geom_ribbon(
      aes(x = horizon, ymin = quant0.4, ymax = quant0.6, alpha = 'Q50'), 
      fill = "blue") + 
    ggplot2::geom_ribbon(
      aes(x = horizon, ymin = quant0.3, ymax = quant0.7, alpha = 'Q50'), 
      fill = "blue") + 
    facet_wrap(~target_type, scales = "free")
  
  
  return(spread_plot)
  
}


#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#'
#' @description 
#' Plots model type performance over periods and horizons
#' Code adapted from scoringutils package 
#'
#' @param pw_comp_dat pairwise comparison data to plot
#' 
#' @references Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S (2022). 
#' “Evaluating Forecasts with scoringutils in R.” 
#' arXiv. doi:10.48550/ARXIV.2205.07090, https://arxiv.org/abs/2205.07090.
#' 
#' 
#' @export

plot_model_type_across_time <- function(pw_comp_dat){
  
  breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
  plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
  
  get_fill_scale <- function(values, breaks, plot_scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values,
                 breaks = breaks,
                 include.lowest = TRUE,
                 right = FALSE,
                 labels = plot_scales
    )
    # scale[is.na(scale)] <- 0
    return(as.numeric(as.character(scale)))
  }
  
  
  comparison_result <- pw_comp_dat |>
    mutate(period = paste0("Period ", period)) |>
    mutate(period = factor(period)) |>
    mutate(var_of_interest = round(mean_scores_ratio, 2),
           fill_col = get_fill_scale(
             var_of_interest,
             breaks, plot_scales)) |>
    data.table()
  
  #comparison_result[, var_of_interest := round(mean_scores_ratio, 2)]
  
  
  #comparison_result[, fill_col := get_fill_scale(
  #  var_of_interest,
  #  breaks, plot_scales
  #)]
  
  high_col <- "salmon"
  
  plot <- ggplot(
    comparison_result,
    aes(
      y = model,
      x = period,
      fill = fill_col
    )
  ) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(label = var_of_interest),
              na.rm = TRUE
    ) +
    scale_fill_gradient2(
      low = "steelblue", mid = "grey95",
      high = high_col,
      na.value = "lightgrey",
      midpoint = 0,
      limits = c(-1, 1),
      name = NULL
    ) +
    theme_scoringutils() +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.position = "none"
    ) +
    labs(
      x = "period", y = ""
    ) +
    coord_cartesian(expand = FALSE) +
    facet_grid(horizon ~ target_type,
               labeller = as_labeller(c(`1` = "1 week ahead", `2` = "2 weeks ahead",
                                        `3` = "3 weeks ahead",`4` = "4 weeks ahead",
                                        `average` = "Average",
                                        `Deaths` = "Target: Deaths",
                                        `Cases` = "Target: Cases")))
  
  return(plot)
}



#' @title COVID-19 Forecast Hub ensemble and model structure analysis
#' 
#' @import dplyr 
#' @import ggplot2
#'
#' @description 
#' Plots model type performance over periods and locs
#' Code adapted from scoringutils package 
#'
#' @param pw_comp_dat pairwise comparison data to plot
#' 
#' @references Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S (2022). 
#' “Evaluating Forecasts with scoringutils in R.” 
#' arXiv. doi:10.48550/ARXIV.2205.07090, https://arxiv.org/abs/2205.07090.
#' 
#' @export
#' 
plot_model_type_across_time_and_loc <- function(pw_comp_dat){
  
  breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
  plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
  
  get_fill_scale <- function(values, breaks, plot_scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values,
                 breaks = breaks,
                 include.lowest = TRUE,
                 right = FALSE,
                 labels = plot_scales
    )
    # scale[is.na(scale)] <- 0
    return(as.numeric(as.character(scale)))
  }
  
  
  comparison_result <- pw_comp_dat |>
    mutate(period = paste0("Period ", period)) |>
    mutate(period = factor(period)) |>
    mutate(var_of_interest = round(mean_scores_ratio, 2),
           fill_col = get_fill_scale(
             var_of_interest,
             breaks, plot_scales)) |>
    data.table()

  high_col <- "salmon"
  
  plot <- ggplot(
    comparison_result,
    aes(
      y = model,
      x = period,
      fill = fill_col
    )
  ) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(label = var_of_interest),
              na.rm = TRUE
    ) +
    scale_fill_gradient2(
      low = "steelblue", mid = "grey95",
      high = high_col,
      na.value = "lightgrey",
      midpoint = 0,
      limits = c(-1, 1),
      name = NULL
    ) +
    theme_scoringutils() +
    theme(
      panel.spacing = unit(1.5, "lines"),
      legend.position = "none"
    ) +
    labs(
      x = "period", y = ""
    ) +
    coord_cartesian(expand = FALSE) +
    facet_grid(location ~ target_type,
               labeller = as_labeller(c(`PL` = "Poland", `DE` = "Germany",
                                        `CZ` = "Czech Rep.", `GB` = "Great Br.",
                                        `FR` = "France",
                                        `Deaths` = "Target: Deaths",
                                        `Cases` = "Target: Cases")))
  
  return(plot)
}


#' @title masterthesis ggplot2 theme
#'
#' @description
#' A theme for ggplot2 plotss
#' @return A ggplot2 theme
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @export
#' 
theme_masterthesis <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
}


#' @title WIS decomp
#'
#' @description
#' WIS decomposition plot
#' @return A ggplot2 theme
#' 
#' @export

wis_decomp_model_type <- function(score_data, 
                                  relative = FALSE,
                                  breaks = c("mechanistic","semi","statistical"),
                                  labels = c("mech.", "semi", "stat.")){
  
  y_lab <- ifelse(relative, "WIS, relative to baseline model",
                  "WIS")
  
  plot <- ggplot(score_data, aes(x = model_type, 
                                 y = value, 
                                 fill = variable)) +
    geom_bar(position="stack", stat="identity") +
    facet_grid(target_type ~ horizon,
               scales = "free_y",
               labeller = as_labeller(c(specs$plot_target_label,
                                        specs$plot_horizon_label))) +
    geom_hline(yintercept = 1) +
    scale_x_discrete(name = "Model Type",
                     breaks = breaks,
                     labels = labels) +
    scale_y_continuous(name = y_lab) +
    scale_fill_brewer(palette = "Set2",
                      labels = c("Overprediction",
                                 "Underprediction",
                                 "Dispersion")) +
    labs(fill = "WIS components") +
    theme_masterthesis()
  
  return(plot)
}




#' @title Overall assessment plot
#'
#' @description
#' Overall assessment plot
#' @import patchwork
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @export

overall_assessment_plot <- function(overall_score_data,
                                    decomp_data,
                                    relative = FALSE,
                                    breaks = c("mechanistic","semi","statistical"),
                                    labels = c("mech.", "semi", "stat.")){
  
  y_lab <- ifelse(relative, "WIS, relative to baseline model",
                  "WIS")
  
  #################wis decomposition plots##################
  #helper function to make plot
  wis_decomp_plot <- function(decomp_data, 
                              target_type){
    wis_plot <- ggplot(decomp_data |>
                        filter(target_type == target_type), 
                      aes(x = model_type, 
                          fill = model_type,
                          y = value,
                          alpha = variable)) +
      geom_bar(position="stack", stat="identity") +
      facet_wrap(~ horizon,
                 labeller = as_labeller(c(specs$plot_horizon_label)),
                 nrow = 1) +
      #geom_hline(yintercept = 1) +
      scale_alpha_discrete(range = c(0.35,1),
                           labels = c("Overprediction",
                                      "Underprediction",
                                      "Dispersion"),
                           name = "Components of the WIS") +
      scale_x_discrete(name = "Model Type",
                       breaks = breaks,
                       labels = labels) +
      scale_y_continuous(name = y_lab) +
      scale_fill_brewer(palette = "Set2", 
                        name = "Model Type")+
      xlab("") +
      #labs(fill = "Model Type") +
      theme_masterthesis() +
      theme(#legend.title=element_blank(),
            legend.position = "none")
    
    return(wis_plot)
  }
  wis_plot_cases <- wis_decomp_plot(decomp_data |>
                                      filter(target_type == "Cases"),
                                    target_type == "Cases")
  wis_plot_deaths <- wis_decomp_plot(decomp_data |>
                                      filter(target_type == "Deaths"),
                                    target_type == "Deaths")
  

  
  ################path plots################
  horizon_path_plot <- function(score_data,
                                score_rule,
                                ylab = score_rule){
    
    path_plot <- ggplot(score_data, 
           aes(x = horizon,
               y = get(score_rule), 
               color = model_type)) +
      scale_color_brewer(palette = "Set2") +
      geom_line() +
      geom_point() +
      facet_wrap(~ target_type,
                 labeller = as_labeller(specs$plot_target_label),
                 scales = "free") +
      ylab(ylab) +
      xlab("Forecast Horizon") +
      guides(fill = "none", colour = "none") +
      theme_masterthesis() %+replace%
      theme(legend.title=element_blank(),
            legend.position = "none")
    
    return(path_plot)
    
  }
  
  cov50_plot <- horizon_path_plot(overall_score_data,
                                  "coverage_50",
                                  ylab = "50% PI Coverage") +
    geom_hline(aes(yintercept = 0.5)) +
    scale_y_continuous(labels = paste0(seq(0, 100, by = 25), "%"),
                       breaks = seq(0,1, by = 0.25),
                       limits = c(0,1)) 


  cov90_plot <- horizon_path_plot(overall_score_data,
                                  "coverage_90",
                                  ylab = "90% PI Coverage") +
    geom_hline(aes(yintercept = 0.9)) +
    scale_y_continuous(labels = paste0(seq(0, 100, by = 25), "%"),
                       breaks = seq(0,1, by = 0.25),
                       limits = c(0,1)) 
  
  bias_plot <- horizon_path_plot(overall_score_data,
                                 "bias",
                                 ylab = "Bias") +
    geom_hline(aes(yintercept = 0)) +
    scale_y_continuous(labels = paste0(c(-0.3,seq(-0.2, 0.2, by = 0.1), 0.3)),
                       breaks = c(-0.3,seq(-0.2, 0.2, by = 0.1), 0.3),
                       limits = c(-0.3,0.3)) 
  
  ae_plot <- horizon_path_plot(overall_score_data,
                                 "ae_median",
                                 ylab = "Absolute Error")
  
  overall_plot <-
    (wis_plot_cases + wis_plot_deaths) /
    (cov50_plot + cov90_plot) /
    (bias_plot + ae_plot) +
  plot_layout(guides = "collect", 
              heights = c(1.3, 1, 1)) &
    plot_annotation(tag_levels = 'I')  &
    theme(legend.position = 'bottom', 
          legend.box="vertical", legend.margin=margin()) 
  
  
  return(overall_plot)

}



best_performers_boxplot <- function(best_performers_data,
                                    avg_points_data,
                                    labeller_targets = specs$plot_target_label,
                                    labeller_locs = specs$plot_location_label){
  plot <- ggplot(best_performers_data, aes(x = factor(nmod),
                                   y = rel_score,
                                   color = factor(nmod))) +
    geom_boxplot(outlier.shape = 20,
                 outlier.color = "darkgray",
                 lwd = 1.05,
                 fatten = 0.8) +
    scale_color_brewer(palette = "Set2",
                       guide = "none") +
    geom_hline(yintercept = 1) +
    facet_grid(target_type ~ location,
               labeller = as_labeller(c(labeller_targets, 
                                        labeller_locs))) +
    theme_masterthesis() +
    ylab("WIS relative to Median Ensemble") +
    xlab("Number of best performers")
  
  
  if(!is.null(avg_points_data)){
    plot <- plot +
      geom_point(data = all_evals_avg, aes(x = factor(nmod), 
                                           y = rel_score),
                 shape = 18, color = "black", size = 2) 
  }
  
  return(plot)
}