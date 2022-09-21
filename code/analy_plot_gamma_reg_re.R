library(here)
library(masterthesis)
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(patchwork)

source(here("specs", "specs.R"))

makereplot <- function(modfit){
  revals <- modfit$mu.coefSmo[[1]]$coefficients$random$inact |> 
    data.table(keep.rownames = TRUE) |>
    rename(re = `(Intercept)`) |>
    separate(rn, into = c("loc", "pcat"), sep = 2) |>
    separate(pcat, into = c("pcat", "target_type"), sep = 1) 
  
  plot <- ggplot(revals, aes(x = as.numeric(pcat), y = re, color = loc)) +
    geom_point(size = 3, shape = 18) +
    geom_line(linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_brewer(palette = "Set2", name = "",
                       breaks = c("PL", "GB", "FR", "DE", "CZ"),
                       labels=c("Poland","United Kingdom","France",
                                "Germany", "Czech R.")) +
    theme_masterthesis() + 
    facet_wrap(~target_type,
               labeller = as_labeller(c(specs$plot_target_label))) +
    ylim(-0.1, 0.2) +
    xlab("Period") +
    ylab("Size of Random Effect") 
    
  
  return(plot)
}

median_model <- readRDS(here("results", "add_model_gamma", "mean_nmod4.RDS"))
mean_model <- readRDS(here("results", "add_model_gamma", "median_nmod4.RDS"))

plot1 <- makereplot(median_model)
plot2 <- makereplot(mean_model)

overall_plot <-
  (plot1 + plot2) +
  plot_layout(guides = "collect", 
              heights = c(1)) &
  plot_annotation(tag_levels = 'I') &
  theme(legend.position = "bottom") 



pdf(here("plots", "gamma_reg_random_effects_nmod4.pdf"), width = 11, height = 4.5)
overall_plot
dev.off()


median_model <- readRDS(here("results", "add_model_gamma", "mean_nmod8.RDS"))
mean_model <- readRDS(here("results", "add_model_gamma", "median_nmod8.RDS"))

plot1 <- makereplot(median_model)
plot2 <- makereplot(mean_model)

overall_plot <-
  (plot1 + plot2) +
  plot_layout(guides = "collect", 
              heights = c(1)) &
  plot_annotation(tag_levels = 'I') &
  theme(legend.position = "bottom") 



pdf(here("plots", "gamma_reg_random_effects_nmod8.pdf"), width = 11, height = 4.5)
overall_plot
dev.off()
