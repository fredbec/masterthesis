library(scoringutils)
library(here)
library(ggplot2)
library(gridExtra)
library(masterthesis)

source(here("code", "load_clean_data.R"))

here::i_am("code/analy_ensemble.R")

locs <- scan(here("specs", "locs.txt"), character(), sep = ",")

cvg_threshold <- scan(here("specs", "cvg_threshold.txt"), numeric())


#make mean and median ensemble and score
hub_data <- hub_data |>
  make_ensemble(summary_function = mean) |>
  make_ensemble(summary_function = median, 
                extra_excl = c("mean_ensemble")) |>
  filter(model %in% c("mean_ensemble", "median_ensemble"))


plotscores <- function(hub_data, loc){
  
  if(length(loc) > 1){
    p_title <- "all"
  } else {
    p_title <- loc
  }
  
  p <- hub_data |>
    filter(location %in% loc) |>
    score() |>
    summarise_scores(by = c("target_type", "model")) |>
    summarise_scores(fun = signif, digits = 2) |>
    plot_score_table(y = "model", by = "target_type") + 
    facet_wrap(~ target_type, nrow = 2, ncol = 1) +
    labs(title = p_title) +
    scale_x_discrete(
      labels=c("IS", "disp","u-pred", "o-pred",
               "cvg_dev", "bias", "ae")) +
    scale_y_discrete(
      labels = c("median", "mean")
    )
  
  return(p)
}

loclist <- c(list(locs), lapply(locs, function(x) c(x)))
plots <- lapply(loclist, function(x) plotscores(hub_data, x))
ggsave(
  filename = here("plots", "score_mean_med.pdf"), 
  plot = marrangeGrob(plots, nrow=3, ncol=2), 
  width = 12, height = 9
)
