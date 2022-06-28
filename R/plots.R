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
    filter(.id %in% which_combs) |>
    #for plotting, looks nicer (i.e. GB.Deaths to GB - Deaths)
    mutate(.id = gsub("\\.", " - ", .id))
  
  
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



plot_kickout_results <- function(model_kickout_results,
                                 random_kickout_results,
                                 target, loc, model_type,
                                 score = "interval_score"){
  
  #stratify by horizon
  random_model_kickout_subset <- random_kickout_results |>
    group_by(model, target_type, location, nmod, sample_id) |>
    summarise_at(names(random_model_kickout_new)[6:12], mean) |>
    ungroup() |>
    filter(model == model_type,
           target_type == target,
           location == loc,
           nmod %in% c(1,2,3,4)) |> 
    group_by(nmod) |>
    summarise(p05 = quantile(get(score), probs = c(0.05)),
              p25 = quantile(get(score), probs = c(0.25)),
              p50 = quantile(get(score), probs = c(0.5)),
              p75 = quantile(get(score), probs = c(0.75)),
              p95 = quantile(get(score), probs = c(0.95)))
  
  
  model_kickout_subset <- model_kickout_results |> 
    filter(nmod > 0,
           model == model_type,
           target_type == target,
           location == loc) |>
    group_by(nmod) |>
    summarise(interval_score = mean(interval_score))
  
  joined_dat <- inner_join(random_model_kickout_subset,
                           model_kickout_subset) |>
    mutate(nmod = factor(nmod))
  
  
  refval <- model_kickout |>
    filter(nmod == 0,
           model == model_type,
           target_type == target,
           location == loc) |>
    summarise(score = mean(get(score))) |>
    pull()
  
  
  myplot <- ggplot(joined_dat,
         aes(x = nmod, y = interval_score, group = 1)) +
    geom_line(color = "purple") +
    geom_line(aes(x = nmod, y = p50), size = 2) +
    geom_ribbon(aes(x = nmod, ymin = p25, ymax = p75), fill = "red", alpha = 0.35) +
    geom_ribbon(aes(x = nmod, ymin = p05, ymax = p95), fill = "red", alpha = 0.15) + 
    geom_hline(aes(yintercept = refval))
  
  print(myplot)
  
    
}