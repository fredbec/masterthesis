library(dplyr)
library(here)
library(data.table)
library(masterthesis)
library(ggplot2)
library(patchwork)

source(here("specs", "specs.R"))

locs <- specs$load_data_locs |> as.list()

read_dat <- function(locs, nmod){
  
  add_mod <- lapply(locs, function(loc) 
    readRDS(here("results", "add_model", paste0("scores_nmod", nmod, "_", loc, ".RDS")))) |>
    rbindlist()
  
  #filter values of ensemble before adding a model
  ens_before <- add_mod |>
    filter(is.na(ens_type)&model%in%c("median_ensemble", 
                                      "mean_ensemble")&is.na(type))
  
  #filter values of ensemble after adding a model
  ens_after <- add_mod |>
    filter(is.na(ens_type)&model%in%c("median_ensemble", 
                                      "mean_ensemble")&!is.na(type))
  
  #get interval score of both sets
  inscore_before <- ens_before |>
    dplyr::select(model, horizon, forecast_date, target_type, location, interval_score, idx) |>
    rename(score_before = interval_score) 
  inscore_after <- ens_after |>
    dplyr::select(model, horizon, forecast_date, target_type, location, type, interval_score, idx) |>
    rename(score_after = interval_score)
  
  #join scores, compute relative score, join period categorization
  joined_scores <- inscore_after |>
    left_join(inscore_before, by = c("model", "horizon", "target_type", 
                                     "location", "forecast_date", "idx")) |>
    mutate(rel_score_to_before = score_after/score_before) |>
    mutate(forecast_date = as.IDate(forecast_date),
           type = factor(type)) |>
    left_join(specs$period_cat_dt, by = c("forecast_date")) |>
    mutate(type = relevel(type, ref = "random_mod"), #random mod should be reference level
           period_cat = relevel(period_cat, ref = "3")) |> #period 3 as reference category
    #make interaction term for random effects
    mutate(inact = paste0(location, period_cat, target_type))
  
  return(joined_scores)
}

  
joined_scores_nmod4 <- read_dat(locs, 4)

joined_scores_nmod8 <- read_dat(list("DE", "PL"), 8)

med_scores <- joined_scores_nmod8 |>
  group_by(target_type, model, type) |>
  summarise(med_wis = median(rel_score_to_before))
  
mean_scores_nmod4 <-  joined_scores_nmod4 |>
  group_by(target_type, model, type) |>
  summarise(rel_score_to_before = mean(rel_score_to_before))


mean_scores_nmod8 <-  joined_scores_nmod8 |>
  group_by(target_type, model, type) |>
  summarise(rel_score_to_before = mean(rel_score_to_before))



make_boxplot <- function(joined_scores, mean_scores){
  plot_scores <- joined_scores |>
    filter(rel_score_to_before < 2) |>
    sample_n(100000) |>
    mutate(type = factor(type,
                         levels = c("best_mod", "worst_mod", "random_mod", "max_dist", "min_dist"),
                         ordered = TRUE))
    
  boxplots <- ggplot(plot_scores |> filter(type %in% c("best_mod", "worst_mod", "random_mod", "max_dist", "min_dist")), 
         aes(x=type, y = rel_score_to_before, color = type)) +
    geom_boxplot(outlier.shape = 20,
                 outlier.alpha = 0.15,
                 outlier.color = "darkgray",
                 lwd = 1.05,
                 fatten = 0.8,
                 position=position_dodge(width =.9)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_masterthesis() +
    theme(axis.text.x=element_blank(),
           axis.text.y = element_text(size = 12)) +
    scale_color_brewer(palette = "Set2",
                       breaks = c("best_mod", "worst_mod", "random_mod", "max_dist", "min_dist"),
                       labels = c("Best model", "Worst model", "Random model", "Max. distance", "Min. distance"),
                       name = "") +
    facet_wrap(target_type ~ model,
                labeller = as_labeller(c(`mean_ensemble` = "Mean Ensemble",
                                         `median_ensemble` = "Median Ensemble",
                                         specs$plot_target_label)),
               nrow = 1) +
    xlab("") +
    ylab("WIS score, relative to before")+
    geom_point(data = mean_scores,
               aes(x = factor(type),
                   y = rel_score_to_before), 
               color = "black",
               shape = 18) 
}
plot1 <- make_boxplot(joined_scores_nmod4, mean_scores_nmod4)
plot2 <- make_boxplot(joined_scores_nmod8, mean_scores_nmod8)

overall_plot <- 
  (plot1) /
  (plot2) +
  plot_layout(guides = "collect", 
              heights = c(1, 1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom') 

pdf(here("plots", "add_model_boxplot.pdf"), width = 10, height = 6.8)
overall_plot
dev.off()






##########################PLOT COND. DISTRIBUTIONS########################
plot_data <- vector(mode = "list", length = 4)

plot_data[[1]] <- joined_scores_nmod4 |>
  filter(type == "random_mod", 
         location == "CZ",
         target_type == "Cases",
         horizon == 2,
         model == "median_ensemble") 
plot_data[[2]] <- joined_scores_nmod4 |>
  filter(type == "best_mod", 
         location == "PL",
         target_type == "Deaths",
         horizon == 1,
         model == "median_ensemble") 
plot_data[[3]] <- joined_scores_nmod4 |>
  filter(type == "worst_mod", 
         location == "FR",
         target_type == "Cases",
         horizon == 3,
         model == "mean_ensemble") 
plot_data[[4]] <- joined_scores_nmod4 |>
  filter(type == "max_dist", 
         location == "DE",
         target_type == "Cases",
         horizon == 4,
         model == "mean_ensemble") 

hist_fun <- function(plotdat){
  plot <- ggplot(plotdat, aes(x = rel_score_to_before))+
    geom_histogram(aes(y = ..density..), binwidth = 0.15,  color="#2d3d62", fill="#8DA0CB") +
    theme_masterthesis() +
    xlab("") +
    ylab("frequency") +
    xlim(0, 5)
  
  return(plot)
}
  
myplots <- lapply(plot_data, hist_fun)

overall_plot <-
  (myplots[[1]] + myplots[[2]]) /
  (myplots[[3]] + myplots[[4]]) +
  plot_layout(guides = "collect", 
              heights = c(1, 1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom')
overall_plot

pdf(here("plots", "appendix", "cond_dists_gamma.pdf"), width = 10, height = 5)
overall_plot
dev.off()