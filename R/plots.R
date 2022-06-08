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