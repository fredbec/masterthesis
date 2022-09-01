library(here)
library(masterthesis)
library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)
library(scales)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))

bpdat <- fread(here("ensembles", "weights", "best_performers_incl_mods.csv"))
bpweights <- fread(here("ensembles", "weights", "best_performers_invscore_weights.csv"))
score_all_mods_with_relwis <- fread(here("score_data", "score_all_mods_with_relwis.csv"))
rel_scores_best_perform <- fread(here("score_data", "rel_scores_best_perform.csv"))
#character NAs not correctly saved
rel_scores_best_perform <- rel_scores_best_perform |>
  mutate(average = ifelse(average == "", NA, "average"))


nm <- 10
dom_mods <- 5

plot_bp_weights <- function(bp_data,
                            bp_weights,
                            nm,
                            loc, 
                            target,
                            dom_mods){
  
  #get dom_mods number of dominant models
  dom_models <- bp_data |>
    filter(nmod == nm) |>
    group_by(location, target_type, model) |>
    summarise(modelcount = n()) |>
    slice_max(n = dom_mods, order_by = modelcount) |>
    mutate(dom_model = 1) |> #to identify dominant models
    select(-modelcount)
  
  
  names_dom_models <- dom_models |>
    filter(location == loc,
           target_type == target) |>
    select(model) |>
    pull()
  
  
  plot_data <- bp_weights |>
    filter(nmod == nm, 
           location == loc,
           target_type == target) |>
    left_join(dom_models, by = c("location", "model", "target_type")) |>
    group_by(forecast_date, location, target_type) |>
    #name all non-dominant models other+counter:
    mutate(counter = 1:n()) |>
    mutate(model = ifelse(is.na(dom_model), paste0("other", counter), model)) |>
    select(-c(dom_model, counter)) |>
    mutate(model = factor(model,
                          levels = c(paste0("other", 1:nm), 
                                     names_dom_models),
                          ordered = TRUE))
  
  #assign colors (other models get grey, dominant models actual color)
  group.cols <- c(paste0("grey", ceiling(seq(50, 75, length.out = nm))),
                  brewer.pal(n=dom_mods,"Set2"))
  names(group.cols) <- c(paste0("other", 1:nm), names_dom_models)
  
  
  plot <- plot_data |>
    ggplot2::ggplot(aes(x = factor(sort(forecast_date)), 
                        y = weights, fill = model)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = group.cols,
                      breaks = c(names_dom_models))  +
    ggplot2::ggtitle("Chosen models") +
    theme_masterthesis() +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1)) +
    xlab("") +
    labs(title = "") +
    ylab("Weights assigned to model")
  
  
}


squiggly_line_plot <- function(rel_score_data,
                               bp_data,
                               dom_mods,
                               nm,
                               loc,
                               target,
                               mindate ="2021-04-12"){
  #get dom_mods number of dominant models
  dom_models <- bp_data |>
    filter(nmod == nm) |>
    group_by(location, target_type, model) |>
    summarise(modelcount = n()) |>
    slice_max(n = dom_mods, order_by = modelcount) |>
    mutate(dom_model = 1) |> #to identify dominant models
    select(-modelcount)
  
  
  names_dom_models <- dom_models |>
    filter(location == loc,
           target_type == target) |>
    select(model) |>
    pull()
  

  #rename all other models to other+counter
  model_desig <- rel_score_data |>
    filter(location == loc,
           target_type == target,
           !grepl("EuroCOVID",model)) |>
    anti_join(dom_models, by = c("location", "model", "target_type")) |>
    select(location, target_type, model) |>
    distinct() |>
    mutate(counter = 1:n()) |>
    mutate(modelname = paste0("other", counter)) |>
    select(-counter)
  
  num_non_dom_models <- length(unique(model_desig$modelname))
  
  group.cols <- c(paste0("grey", rep(80, times = num_non_dom_models)),
                  brewer.pal(n=dom_mods,"Set2"))

  names(group.cols) <- c(paste0("other", 1:num_non_dom_models), names_dom_models)
  
  group.size <- c(rep(1, times = num_non_dom_models), rep(2, times = dom_mods))
  names(group.size) <- c(paste0("other", 1:num_non_dom_models), names_dom_models)
  group.alpha <- c(rep(0.5, times = num_non_dom_models), rep(1, times = dom_mods))
  names(group.alpha) <- c(paste0("other", 1:num_non_dom_models), names_dom_models)
  

  plot_data <- rel_score_data |>
    filter(location == loc,
           target_type == target,
           !grepl("EuroCOVID",model),
           forecast_date >= as.Date(mindate)) |>
    left_join(model_desig, by = c("location", "model", "target_type")) |>
    mutate(model = ifelse(is.na(modelname), model, modelname)) |> #replace with other if non dominant model
    arrange(forecast_date) |>
    select(-modelname) 

  plot_dat_dom <- plot_data |>
    filter(!grepl("other", model))
  plot_non_dom <- plot_data |>
    filter(grepl("other", model))
  
  
  plot <- ggplot(plot_data,
                 aes(x = forecast_date,
                     y = scaled_rel_skill,
                     color = model))  +
    geom_line(aes(size = model,
                  alpha = model)) +
    scale_size_manual(values = group.size,
                      breaks = c(names_dom_models),
                      name = "") +
    scale_alpha_manual(values = group.alpha,
                       breaks = c(names_dom_models),
                       name = "") +
    scale_color_manual(values = group.cols,
                      breaks = c(names_dom_models),
                      name = "") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_masterthesis() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %y",
                 expand = c(0,0)) +
    #scale_x_date(expand = c(0,0)) +
    xlab("") +
    ylab("relative WIS (log scale)") +
    scale_y_continuous(trans = "log2")
  
  return(plot)
    
  
  
}


plot_rel_score_over_time <- function(eval_data,
                                     nm,
                                     loc,
                                     target,
                                     ylims = c(0,2.3)){
  
  plot_data <- eval_data |>
    filter(nmod == nm,
           location %in% loc,
           target_type == target,
           model == "weighted.median_ensemble",
           is.na(average)) |>
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
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %y",
                 expand = c(0,0)) +
    ylab(expression("avg. WIS relative to \n unweighted median")) +
    xlab("") +
    ylim(ylims)
  
  return(plot)
}

plot_predictions_bp <- function(bp_data,
                            hub_data,
                            nm,
                            loc, 
                            target,
                            dom_mods,
                            ylimval){
  point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
  
  
  #get dom_mods number of dominant models
  dom_models <- bp_data |>
    filter(nmod == nm) |>
    group_by(location, target_type, model) |>
    summarise(modelcount = n()) |>
    slice_max(n = dom_mods, order_by = modelcount) |>
    mutate(dom_model = 1) |> #to identify dominant models
    select(-modelcount)
  
  
  names_dom_models <- dom_models |>
    filter(location == loc,
           target_type == target) |>
    select(model) |>
    pull()
  
  
  model_desig <- hub_data |>
    filter(location == loc,
           target_type == target,
           !grepl("EuroCOVID",model)) |>
    anti_join(dom_models, by = c("location", "model", "target_type")) |>
    select(location, target_type, model) |>
    distinct() |>
    mutate(counter = 1:n()) |>
    mutate(modelname = paste0("other", counter)) |>
    select(-counter)
  
  num_non_dom_models <- length(unique(model_desig$modelname))
  
  plot_data <- hub_data |>
    filter(location == loc,
           target_type == target,
           quantile == 0.5,
           horizon == 2,
           !grepl("EuroCOVID",model)) |>
    select(model, location, target_type, target_end_date, true_value, prediction, forecast_date) |>
    left_join(model_desig, by = c("location", "model", "target_type")) |>
    mutate(model = ifelse(is.na(modelname), model, modelname)) |> #replace with other if non dominant model
    arrange(forecast_date) |>
    select(-modelname) 
  
  
  
  #make dataset long and such that true_value is just another "model" with 
  #according "prediction"
  truevals <- plot_data |>
    select(target_end_date, true_value) |>
    distinct() |>
    mutate(model = "True value") |>
    rename(prediction = true_value) |>
    mutate(prediction = prediction/1000)
  
  model_data <- plot_data |>
    select(model, target_end_date, prediction) |>
    melt(id.var = c("model", "target_end_date")) |>
    select(-variable) |>
    rename(prediction = value) |>
    mutate(prediction = prediction/1000)
  
  
  plot_data <- rbind(truevals, model_data)
  #assign colors (other models get grey, dominant models actual color)
  group.cols <- c(paste0("grey", ceiling(seq(50, 75, length.out = nm))),
                  brewer.pal(n=dom_mods,"Set2"),
                  "black")
  names(group.cols) <- c(paste0("other", 1:nm), c(names_dom_models), "True value")
  
  group.size <- c(rep(1, times = num_non_dom_models), rep(2, times = dom_mods), 0.75)
  names(group.size) <- c(paste0("other", 1:num_non_dom_models), c(names_dom_models, "True value"))
  group.alpha <- c(rep(0.3, times = num_non_dom_models), rep(1, times = dom_mods + 1))
  names(group.alpha) <- c(paste0("other", 1:num_non_dom_models),  c(names_dom_models, "True value"))
  
  
  plot <- plot_data |>
    ggplot2::ggplot(aes(x = target_end_date,
                        y = prediction,
                        color = model,
                        size = model,
                        alpha = model)) +
    geom_line() +
    geom_point(data = truevals) +
    scale_size_manual(values = group.size,
                      breaks = c("True value"),
                      name = "") +
    scale_alpha_manual(values = group.alpha,
                       breaks = c( "True value"),
                       name = "") +
    scale_color_manual(values = group.cols,
                       breaks = c("True value"),
                       name = "") +
    theme_masterthesis() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %y",
                 expand = c(0,0)) +
    ylab(expression("Number of predicted Cases \n    (in thousands)")) +
    xlab("") +
    ylim(0,ylimval)
  
  return(plot)
  
}


plot3 <- plot_bp_weights(bpdat, bpweights, nm, "PL", "Cases", dom_mods)
plot2 <- squiggly_line_plot(score_all_mods_with_relwis, bpdat, dom_mods, nm, "PL", "Cases")
plot1 <- plot_rel_score_over_time(rel_scores_best_perform, nm, loc = c("PL"), "Cases")
plot0 <- plot_predictions_bp(bpdat, hub_data, nm, "PL", "Cases", dom_mods, ylimval = 550)


overall_plot_pl <-
  (plot1) /
  (plot0) /
  (plot2) /
  (plot3) +
  plot_layout(guides = "keep", 
              heights = c(0.3,0.85,0.75,1.5)) &
  plot_annotation(tag_levels = 'I') 
#this shows two problems with the data:
#different models work differently at different locations, and we have different models altogether
pdf(here("plots", "best_performers_weights_pl.pdf"), 
    height = 16, width = 12)
overall_plot_pl
dev.off()


plot3 <- plot_bp_weights(bpdat, bpweights, nm, "DE", "Cases", dom_mods)
plot2 <- squiggly_line_plot(score_all_mods_with_relwis, bpdat, dom_mods, nm, "DE", "Cases")
plot1 <- plot_rel_score_over_time(rel_scores_best_perform, nm, loc = c("DE"), "Cases")
plot0 <- plot_predictions_bp(bpdat, hub_data, nm, "DE", "Cases", dom_mods, ylimval = 1300)


overall_plot_de <-
  (plot1) /
  (plot0) /
  (plot2) /
  (plot3) +
  plot_layout(guides = "keep", 
              heights = c(0.3,1,0.85,1.5)) &
  plot_annotation(tag_levels = 'I') 
#this shows two problems with the data:
#different models work differently at different locations, and we have different models altogether
pdf(here("plots", "best_performers_weights_de.pdf"), 
    height = 16, width = 12)
overall_plot_de
dev.off()




plot3 <- plot_bp_weights(bpdat, bpweights, 5, "GB", "Deaths", dom_mods)
plot2 <- squiggly_line_plot(score_all_mods_with_relwis, bpdat, dom_mods, 5, "GB", "Deaths")
plot1 <- plot_rel_score_over_time(rel_scores_best_perform, 5, loc = c("GB"), "Deaths")
plot0 <- plot_predictions_bp(bpdat, hub_data, 5, "GB", "Deaths", dom_mods, ylimval = 4.5)



#Great Britain and Deaths
overall_plot_gb <-
  (plot1) /
  (plot0) /
  (plot2) /
  (plot3) +
  plot_layout(guides = "keep", 
              heights = c(0.3,0.85,0.75,1.5)) &
  plot_annotation(tag_levels = 'I') 
#this shows two problems with the data:
#different models work differently at different locations, and we have different models altogether
pdf(here("plots", "best_performers_weights_gbdeaths.pdf"), 
    height = 16, width = 12)
overall_plot_gb
dev.off()





#Why does it work better in Poland?? consistently high weight for a model?