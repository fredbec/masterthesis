library(masterthesis)
library(ggplot2)
library(dplyr)
library(patchwork)

source(here("specs", "specs.R"))

devtools::load_all()

median_ens <- 
  data.table::fread(here("ensembles", "median_ensemble.csv"))

excl_from_bp <- 
  data.table::fread(here("ensembles", "excl_from_bp.csv"))

all_evals <- NULL
for(nm in c(3,5,8,10)){
  bestperforms_mean <- data.table::fread(here("ensembles", 
                                              paste0("best_performers_ensemble_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)
  
  bestperforms_median <- data.table::fread(here("ensembles", 
                                                paste0("best_performers_ensemble_median_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)
  
  bestperforms_invscore_mean <- data.table::fread(here("ensembles", 
                                              paste0("best_performers_ensemble_invscore_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)
  
  bestperforms_invscore_median <- data.table::fread(here("ensembles", 
                                                paste0("best_performers_ensemble_invscore_median_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)
  
  
  
  eval_mean <- fast_eval(bestperforms_mean, median_ens, 
                         strat_by = c("model", "location", "target_type", "forecast_date"),
                         return_eval = TRUE,
                         comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)
  
  
  eval_median <- fast_eval(bestperforms_median, median_ens, 
                           strat_by = c("model", "location", "target_type", "forecast_date"),
                           return_eval = TRUE,
                           comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)
  
  
  eval_invscore_mean <- fast_eval(bestperforms_invscore_mean, median_ens, 
                         strat_by = c("model", "location", "target_type", "forecast_date"),
                         return_eval = TRUE,
                         comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)
  
  
  eval_invscore_median <- fast_eval(bestperforms_invscore_median, median_ens, 
                           strat_by = c("model", "location", "target_type", "forecast_date"),
                           return_eval = TRUE,
                           comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)
  
  
  all_evals <- all_evals |>
    rbind(eval_mean) |>
    rbind(eval_median) |>
    rbind(eval_invscore_mean) |>
    rbind(eval_invscore_median)
}


all_evals_na <- all_evals |> 
  filter(!is.na(forecast_date), #this is only to exclude average values(in next dataframe)
         rel_score < 3,
         model == "median_ensemble")
#get average values
all_evals_avg <- all_evals |> 
  filter(!is.na(average), 
         rel_score < 3, 
         is.na(forecast_date),
         model == "median_ensemble")
plot1 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(specs$plot_location_label, 
                                                   Average = "Average"))

all_evals_na <- all_evals |> 
  filter(!is.na(forecast_date), #this is only to exclude average values (in next dataframe)
         rel_score < 3,
         model == "mean_ensemble")
#get average values
all_evals_avg <- all_evals |> 
  filter(!is.na(average), 
         rel_score < 3, 
         is.na(forecast_date),
         model == "mean_ensemble")
plot2 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(specs$plot_location_label, 
                                                   Average = "Average"))

all_evals_na <- all_evals |> 
  filter(!is.na(forecast_date), #this is only to exclude average values(in next dataframe)
         rel_score < 3,
         model == "weighted.median_ensemble")
#get average values
all_evals_avg <- all_evals |> 
  filter(!is.na(average), 
         rel_score < 3, 
         is.na(forecast_date),
         model == "weighted.median_ensemble")
plot3 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(specs$plot_location_label, 
                                                   Average = "Average"))

all_evals_na <- all_evals |> 
  filter(!is.na(forecast_date), #this is only to exclude average values (in next dataframe)
         rel_score < 3,
         model == "weighted.mean_ensemble")
#get average values
all_evals_avg <- all_evals |> 
  filter(!is.na(average), 
         rel_score < 3, 
         is.na(forecast_date),
         model == "weighted.mean_ensemble")
plot4 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(specs$plot_location_label, 
                                                   Average = "Average"))

overall_plot <-
  (plot1) /
  (plot2) /
  (plot3) /
  (plot4)+
  plot_layout(guides = "collect", 
              heights = c(1, 1,1,1)) &
  plot_annotation(tag_levels = 'I') 

pdf(here("plots", "best_performers_boxplot2.pdf"),
    height = 15, width = 12)
overall_plot
dev.off()