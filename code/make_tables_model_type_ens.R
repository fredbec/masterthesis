library(here)
library(masterthesis)
library(dplyr)
library(scoringutils)
library(kableExtra)
library(data.table)
library(xtable)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

devtools::load_all()

median_ens <- fread(here("ensembles", "median_ensemble_dom_model_types.csv"))
#semi_rem_ens <- fread(here("ensembles", "semi_remove_ensemble.csv"))

invscore_expsmooth <- fread(here("ensembles",
  "invscore_weights_model_level_median_ensemble_expsmooth.csv"))
qra_model <- fread(here("ensembles", "qra_model_type_ensemble.csv"))
qra_model_adj <- fread(here("ensembles", "qra_model_type_adj_ensemble.csv"))

rel_score_qra_mod <- fast_eval(qra_model, median_ens,
                                return_eval = TRUE,
                               comp_avg_by = c("forecast_date", "target_type")) |>
  mutate(location = ifelse(is.na(location), "Average", location)) 

agg_scores_qra <- rel_score_qra_mod |>
  group_by(model, location, target_type) |>
  summarise(tmean = mean(target_val),
            cmean = mean(current_val)) |>
  mutate(rel_wis = tmean/cmean) |>
  arrange(target_type) |>
  mutate(model = "QRA")


rel_score_qra_adj <- fast_eval(qra_model_adj, median_ens,
                               return_eval = TRUE,
                               comp_avg_by = c("forecast_date", "target_type")) |>
  mutate(location = ifelse(is.na(location), "Average", location)) 

agg_scores_qra_adj <- rel_score_qra_adj |>
  group_by(model, location, target_type) |>
  summarise(tmean = mean(target_val),
            cmean = mean(current_val)) |>
  mutate(rel_wis = tmean/cmean) |>
  arrange(target_type) |>
  mutate(model = "QRA - adj. weights")


rel_score_invscore <- fast_eval(invscore_expsmooth, median_ens,
                                return_eval = TRUE,
                                comp_avg_by = c("forecast_date", "target_type")) |>
  mutate(location = ifelse(is.na(location), "Average", location))

agg_scores_invscore <- rel_score_invscore |>
  group_by(model, location, target_type) |>
  summarise(tmean = mean(target_val),
            cmean = mean(current_val)) |>
  mutate(rel_wis = tmean/cmean) |>
  arrange(target_type) |>
  mutate(model = "Inverse score weighted median")

all_agg_scores <- rbind(agg_scores_invscore, 
                        agg_scores_qra, 
                        agg_scores_qra_adj)


make_tex_table <- function(agg_scores, modelname){
  nrow_tt <- unique(agg_scores$model)
  
  agg_scores <- agg_scores |>
    #mutate(model = modelname) |>
    mutate(location = factor(location,
                             levels = c("Average", "CZ", "DE", "FR", "GB", "PL"),
                             labels = c("Average", "Czech R.", "Germany", "France", "U.K.", "Poland")))|>
    select(target_type, model, location, rel_wis) |>
    mutate(rel_wis = round(rel_wis, 3)) |>
    setDT() |>
    dcast(target_type + model~ location, value.var = "rel_wis") |>
    select(target_type, model, everything()) |>
    arrange(target_type) |>
    select(target_type, everything())
    
  return(agg_scores)
}

tex_table <- make_tex_table(all_agg_scores)


mytab <- xtable(tex_table, digits = 3)
print(mytab, file = "thesis/tables/model_type_ens_against_all.tex")