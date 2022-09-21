library(here)
library(masterthesis)
library(ggplot2)
library(scoringutils)
library(patchwork)
library(dplyr)
library(data.table)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))

devtools::load_all()

weight_inv <- fread(here("ensembles", "weights", "invscore_weights_model_level_expsmooth.csv"))
weight_inv_ens <- fread(here("ensembles", "invscore_weights_model_level_median_ensemble_expsmooth.csv"))
median_ens <- fread(here("ensembles", "median_ensemble_dom_model_types.csv"))

weights_qra <- fread(here("ensembles", "weights", "qra_model_type_weights.csv"))

eval_ens <- fast_eval(weight_inv_ens, median_ens,
                  strat_by = c("model", "location", "target_type", "forecast_date"),
                  return_eval = TRUE)


plot_rel_score_over_time <- function(eval_data,
                                     loc,
                                     target,
                                     ylims = c(0,2.3),
                                     period_cat = specs$period_cat){
                                       
 periodcat <- specs$period_cat |>
   lapply(function(x) c(x[1])) |>
   unlist() |>
   as.IDate(origin = "1970-01-01")
 
  plot_data <- eval_data |>
    filter(location %in% loc,
           target_type == target,
           model == "weighted.median_ensemble") |>
    arrange(location, forecast_date) |>
    group_by(location) |> #ensures lags/leads are correctly computed
    mutate(lagg = lag(rel_score),
           leadd =lead(rel_score)) |>
    mutate(lagg = ifelse(is.na(lagg), rel_score, lagg),
           leadd = ifelse(is.na(leadd), rel_score, leadd)) |>
    mutate(smoothval = (lagg + leadd + rel_score)/3)
  
  
  plot <- ggplot(plot_data,
                 aes(x = forecast_date,
                     y = smoothval,
                     color = location)) +
    geom_line() +
    scale_color_brewer(palette = "Set2",
                       name = "",
                       guide = "none") +
    geom_hline(yintercept = 1, 
               linetype = "dashed") +
    theme_masterthesis() +
    scale_x_continuous(breaks = periodcat,
                       labels = paste0("period", seq(1,5))) +
    ylab(expression("avg. WIS relative to \n unweighted median")) +
    xlab("") +
    ylim(ylims)
  
  return(plot)
}

plot_weights <- function(weight_data,
                         hub_data,
                         loc,
                         target,
                         qra = FALSE,
                         agg = TRUE, #are weights at level of model
                         period_cat = specs$period_cat){
  periodcat <- specs$period_cat |>
    lapply(function(x) c(x[1])) |>
    unlist() |>
    as.IDate(origin = "1970-01-01")
  
  
  periodcat[1] <- min(weight_inv$forecast_date)
  
  if(agg & !qra){
    modtype <- hub_data |> 
      select(model, model_type) |>
      distinct()
    
    weight_data <- weight_data |>
      left_join(modtype, by = "model")
  }
  
  if(qra){
    weight_data <- weight_data |>
      group_by(location, target_type, forecast_date, model) |>
      summarise(weights = sum(weights)) |>
      mutate(forecast_date = as.IDate(forecast_date)) |>
      rename(model_type = model)
    
  } else {
  
  weight_data <- weight_data |>
    group_by(location, target_type, forecast_date, model_type) |>
    summarise(weights = sum(weights)) |>
    mutate(forecast_date = as.IDate(forecast_date)) 
  }
  
  plot_data <- weight_data |>
    filter(location == loc, 
           target_type == target)
  
  plot <- plot_data |>
    ggplot2::ggplot(aes(x = (sort(forecast_date)), 
                        y = weights, fill = model_type)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::ggtitle("Chosen models") +
    theme_masterthesis() +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2", name ="") +
    scale_x_continuous(breaks = periodcat,
                       labels = paste0("period", seq(1,5))) +
    xlab("") +
    labs(title = "") +
    ylab("Weights assigned to model")
  
  return(plot)
}

plot1 <- plot_rel_score_over_time(eval_ens, "PL", "Deaths")
plot2 <- plot_weights(weight_inv, hub_data, "PL", "Deaths")

overall_plot <-
  (plot1) /
  (plot2) +
  plot_layout(guides = "collect", 
              heights = c(1, 1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom', 
        legend.box="vertical", legend.margin=margin()) 


pdf(here("plots", "mt_weights_pldeaths.pdf"),
    width = 10, height = 5)
overall_plot
dev.off()

############################RElative Overall Scores############################


rel_score_invscore <- fast_eval(weight_inv_ens, median_ens,
                                return_eval = TRUE,
                                comp_avg_by = c("period_cat", "target_type", "location", "horizon"),
                                period_cat_dt = specs$period_cat_dt) |>
  mutate(location = ifelse(is.na(location), "Average", location)) |>
  filter(average == "average")

pdf(here("plots", "tile_plot_model_type_ens.pdf"),
    width = 10, height = 5)
tile_plot_rel_score(rel_score_inverse)
dev.off()





rel_score_invscore_avg <- fast_eval(weight_inv_ens, median_ens,
                                    return_eval = TRUE,
                                    comp_avg_by = c("period_cat", "target_type", "location"),
                                    period_cat_dt = specs$period_cat_dt) |>
  mutate(location = ifelse(is.na(location), "Average", location)) |>
  filter(average == "average")

pdf(here("plots", "tile_plot_model_type_ens_avg.pdf"),
    width = 12, height = 2.5)
tile_plot_rel_score_avg(rel_score_invscore_avg)
dev.off()



mod_types <- hub_data |>
  select(model, model_type) |>
  distinct()

weight_inv_avg <- weight_inv |>
  left_join(mod_types, by = "model")|>
  group_by(model_type, target_type, location, forecast_date) |>
  summarise(weights = sum(weights)) |>
  left_join(specs$period_cat_dt, by = "forecast_date") |>
  group_by(model_type, target_type, location, period_cat) |>
  summarise(meanw = mean(weights))

  
plot1 <- tile_plot_rel_score_avg(rel_score_invscore_avg) +
  ggtitle("WIS relative to benchmark")
plot2 <- tile_plot_weights(weight_inv_avg) +
  ggtitle("Average Weights by Model Type and Period")


overall_plot <-
  (plot1) /
  (plot2) +
  plot_layout(guides = "collect", 
              heights = c(1, 4)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.box="vertical", legend.margin=margin()) 

pdf(here("plots", "tile_plot_model_type_ens_avg.pdf"),
    width = 11.5, height = 8)
overall_plot
dev.off()




#######################PLOTS of QRA weights####################
plot11 <- plot_weights(weights_qra, hub_data, "GB", "Deaths", qra = TRUE)
plot21 <- plot_weights(weights_qra, hub_data, "DE", "Deaths", qra = TRUE)
plot31 <- plot_weights(weights_qra, hub_data, "FR", "Cases", qra = TRUE)
plot41 <- plot_weights(weights_qra, hub_data, "PL", "Cases", qra = TRUE)

plot12 <- plot_weights(weight_inv, hub_data, "GB", "Deaths")
plot22 <- plot_weights(weight_inv, hub_data, "DE", "Deaths")
plot32 <- plot_weights(weight_inv, hub_data, "FR", "Cases")
plot42 <- plot_weights(weight_inv, hub_data, "PL", "Cases")


overall_plot <-
  (plot11 + plot12) /
  (plot21 + plot22) /
  (plot31 + plot32) /
  (plot41 + plot42) +
  plot_layout(heights = c(1, 1, 1, 1)) &
  plot_annotation(guides = "collect",
                  tag_levels = 'I')  &
  theme(legend.box="vertical", legend.margin=margin(),
        legend.position = "bottom") 

pdf(here("plots", "appendix", "qra_weights.pdf"),
    width = 11.5, height = 13)
overall_plot
dev.off()
