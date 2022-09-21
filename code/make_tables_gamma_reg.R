library(here)
library(masterthesis)
library(dplyr)
library(gamlss)
library(tidyr)
library(xtable)
library(knitr)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))


make_reg_table <- function(summary.model){
  reg_tab <- summary.model |> 
    data.table(keep.rownames = TRUE) |>
    dplyr::select(all_of(c("rn", "Estimate", "Std. Error"))) |>
    rename(coefficient = rn) |>
    mutate(param = c(rep("mu", 6), rep("sigma", 6))) |>
    mutate(coefficient = factor(coefficient, 
                                levels = c("(Intercept)", "typebest_mod",
                                           "typeworst_mod", "typemax_dist", 
                                           "typemin_dist", "horizon"),
                                labels = c("intercept", "Best Model", 
                                           "Worst Model", "Max. Distance Model",
                                           "Min. Distance Model", "Horizon"),
                                ordered = TRUE)) |>
    arrange(param, coefficient) |>
    melt(measure.vars = c("Estimate", "Std. Error")) |>
    arrange(param, coefficient, variable)
  
  return(reg_tab)
}

make_full_table <- function(dt_median, dt_mean, 
                            caption = NULL,
                            label = NULL){
  
  full_table <- dt_mean |>
    left_join(dt_median, by = c("coefficient", "param", "variable")) |>
    rename(`Mean Ens.` = value.x,
           `Med Ens.` = value.y) |>
    mutate(param = ifelse(param == "mu", "mu", "sig")) |>
    dcast(coefficient + variable ~ param, value.var = c("Med Ens.",
                                                        "Mean Ens.")) |>
    dplyr::select(all_of(c("coefficient", "variable", "Med Ens._mu", "Mean Ens._mu",
                           "Med Ens._sig", "Mean Ens._sig"))) |>
    xtable(digits = 4, caption = caption, label = label)
  
  return(full_table)
}


###base results k = 4
summary.median_model <- readRDS(here("results", "add_model_gamma", "median_nmod4_summary.RDS"))
summary.mean_model <- readRDS(here("results", "add_model_gamma", "mean_nmod4_summary.RDS"))

dt_median <- make_reg_table(summary.median_model) 
dt_mean <- make_reg_table(summary.mean_model)

fulltab <- make_full_table(dt_median, dt_mean, 
                           caption = "Results from gamma reg, k = 4",
                           label = "tab:gamma_results_nmod4")
print(fulltab, type = "latex", 
      table.placement = "t",
      file = here("thesis", "tables", "gamma_results_nmod4.tex"),
      include.rownames = FALSE,
      hline.after = c(-1, 0, nrow(fulltab)))


###sens results k = 4
summary.median_model <- readRDS(here("results", "add_model_gamma", "median_nmod4_summary_sens.RDS"))
summary.mean_model <- readRDS(here("results", "add_model_gamma", "mean_nmod4_summary_sens.RDS"))

dt_median <- make_reg_table(summary.median_model) 
dt_mean <- make_reg_table(summary.mean_model)

fulltab <- make_full_table(dt_median, dt_mean,
                           caption = "Results from gamma reg without outliers, k = 4",
                           label = "tab:gamma_results_nmod4_sens")
print(fulltab, type = "latex", 
      table.placement = "t",
      file = here("thesis", "tables", "gamma_results_nmod4_sens.tex"),
      include.rownames = FALSE,
      hline.after = c(-1, 0, nrow(fulltab)))


#results for pl, de
summary.median_model <- readRDS(here("results", "add_model_gamma", "median_nmod4_plde_summary.RDS"))
summary.mean_model <- readRDS(here("results", "add_model_gamma", "mean_nmod4_plde_summary.RDS"))

dt_median <- make_reg_table(summary.median_model) 
dt_mean <- make_reg_table(summary.mean_model)

fulltab <- make_full_table(dt_median, dt_mean,
                           caption = "Results from gamma reg PL DE, k = 4",
                           label = "tab:gamma_results_nmod4_plde")
print(fulltab, type = "latex", 
      table.placement = "t",
      file = here("thesis", "tables", "gamma_results_nmod4_plde.tex"),
      include.rownames = FALSE,
      hline.after = c(-1, 0, nrow(fulltab)))


# results for k = 8
summary.median_model <- readRDS(here("results", "add_model_gamma", "median_nmod8_summary.RDS"))
summary.mean_model <- readRDS(here("results", "add_model_gamma", "mean_nmod8_summary.RDS"))

dt_median <- make_reg_table(summary.median_model) 
dt_mean <- make_reg_table(summary.mean_model)

fulltab <- make_full_table(dt_median, dt_mean,
                           caption = "Results from gamma reg, k = 8",
                           label = "tab:gamma_results_nmod8")
print(fulltab, type = "latex", 
      table.placement = "t",
      file = here("thesis", "tables", "gamma_results_nmod8.tex"),
      include.rownames = FALSE,
      hline.after = c(-1, 0, nrow(fulltab)))
