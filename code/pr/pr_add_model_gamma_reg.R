library(here)
library(masterthesis)
library(dplyr)
library(gamlss)
library(tidyr)
library(xtable)
library(knitr)

source(here("specs", "specs.R"))
source(here("code", "load_clean_data.R"))


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


############################NMOD 4###################################

joined_scores_nmod <- read_dat(locs, 4)

#get median and mean scores
medscores <- joined_scores_nmod |> 
  filter(model == "median_ensemble")
meanscores <- joined_scores_nmod |> 
  filter(model == "mean_ensemble")


#fit gamlss models
median_model <- gamlss(rel_score_to_before ~ type + horizon +
                         re(random = ~1|inact), 
                       sigma.formula = ~ type + horizon +
                         re(random = ~1|inact),
                       family = GA(mu.link = "identity"),
                       data = medscores)
mean_model <- gamlss(rel_score_to_before ~ type + horizon + 
                       re(random = ~1|inact), 
                     #sigma.formula = ~ type + horizon +
                    #   re(random = ~1|inact),
                     family = GA(mu.link = "identity"),
                     data = meanscores)
summary.median_model <- summary(median_model)
summary.mean_model <- summary(mean_model)

saveRDS(median_model, 
        here("results", "add_model_gamma", "median_nmod4.RDS"))
saveRDS(summary.median_model, 
        here("results", "add_model_gamma", "median_nmod4_summary.RDS"))

saveRDS(mean_model, 
        here("results", "add_model_gamma", "mean_nmod4.RDS"))
saveRDS(summary.mean_model, 
        here("results", "add_model_gamma", "mean_nmod4_summary.RDS"))


#exclude outliers
joined_scores_nmod <- read_dat(locs, 4) |>
  filter(rel_score_to_before < 3)

#get median and mean scores
medscores <- joined_scores_nmod |> 
  filter(model == "median_ensemble")
meanscores <- joined_scores_nmod |> 
  filter(model == "mean_ensemble")


#fit gamlss models
median_model <- gamlss(rel_score_to_before ~ type + horizon +
                         re(random = ~1|inact), 
                       sigma.formula = ~ type + horizon +
                         re(random = ~1|inact),
                       family = GA(mu.link = "identity"),
                       data = medscores)
mean_model <- gamlss(rel_score_to_before ~ type + horizon + 
                       re(random = ~1|inact), 
                     sigma.formula = ~ type + horizon +
                       re(random = ~1|inact),
                     family = GA(mu.link = "identity"),
                     data = meanscores)
summary.median_model <- summary(median_model)
summary.mean_model <- summary(mean_model)

saveRDS(median_model, 
        here("results", "add_model_gamma", "median_nmod4_sens.RDS"))
saveRDS(summary.median_model, 
        here("results", "add_model_gamma", "median_nmod4_summary_sens.RDS"))
saveRDS(mean_model, 
        here("results", "add_model_gamma", "mean_nmod4_sens.RDS"))
saveRDS(summary.mean_model, 
        here("results", "add_model_gamma", "mean_nmod4_summary_sens.RDS"))



##########for nmod = 8 #####################

locs <- as.list(c("DE", "PL"))

joined_scores_nmod <- read_dat(locs, 8)

#get median and mean scores
medscores <- joined_scores_nmod |> 
  filter(model == "median_ensemble")
meanscores <- joined_scores_nmod |> 
  filter(model == "mean_ensemble")


#fit gamlss models
median_model <- gamlss(rel_score_to_before ~ type + horizon +
                         re(random = ~1|inact), 
                       sigma.formula = ~ type + horizon +
                         re(random = ~1|inact),
                       family = GA(mu.link = "identity"),
                       data = medscores)
mean_model <- gamlss(rel_score_to_before ~ type + horizon + 
                       re(random = ~1|inact), 
                     sigma.formula = ~ type + horizon +
                       re(random = ~1|inact),
                     family = GA(mu.link = "identity"),
                     data = meanscores)
summary.median_model <- summary(median_model)
summary.mean_model <- summary(mean_model)

saveRDS(median_model, 
        here("results", "add_model_gamma", "median_nmod8.RDS"))
saveRDS(summary.median_model, 
        here("results", "add_model_gamma", "median_nmod8_summary.RDS"))
saveRDS(mean_model, 
        here("results", "add_model_gamma", "mean_nmod8.RDS"))
saveRDS(summary.mean_model, 
        here("results", "add_model_gamma", "mean_nmod8_summary.RDS"))



######without outliers###############
joined_scores_nmod <- read_dat(locs, 8) |>
  filter(rel_score_to_before < 3)

#get median and mean scores
medscores <- joined_scores_nmod |> 
  filter(model == "median_ensemble")
meanscores <- joined_scores_nmod |> 
  filter(model == "mean_ensemble")


#fit gamlss models
median_model <- gamlss(rel_score_to_before ~ type + horizon +
                         re(random = ~1|inact), 
                       sigma.formula = ~ type + horizon +
                         re(random = ~1|inact),
                       family = GA(mu.link = "identity"),
                       data = medscores)
mean_model <- gamlss(rel_score_to_before ~ type + horizon + 
                       re(random = ~1|inact), 
                     sigma.formula = ~ type + horizon +
                       re(random = ~1|inact),
                     family = GA(mu.link = "identity"),
                     data = meanscores)
summary.median_model <- summary(median_model)
summary.mean_model <- summary(mean_model)

saveRDS(median_model, 
        here("results", "add_model_gamma", "median_nmod8_sens.RDS"))
saveRDS(summary.median_model, 
        here("results", "add_model_gamma", "median_nmod8_summary_sens.RDS"))
saveRDS(mean_model, 
        here("results", "add_model_gamma", "mean_nmod8_sens.RDS"))
saveRDS(summary.mean_model, 
        here("results", "add_model_gamma", "mean_nmod8_summary_sens.RDS"))




joined_scores_nmod <- read_dat(locs, 4)

#get median and mean scores
medscores <- joined_scores_nmod |> 
  filter(model == "median_ensemble")
meanscores <- joined_scores_nmod |> 
  filter(model == "mean_ensemble")


#fit gamlss models
median_model <- gamlss(rel_score_to_before ~ type + horizon +
                         re(random = ~1|inact), 
                       sigma.formula = ~ type + horizon +
                         re(random = ~1|inact),
                       family = GA(mu.link = "identity"),
                       data = medscores)
mean_model <- gamlss(rel_score_to_before ~ type + horizon + 
                       re(random = ~1|inact), 
                     sigma.formula = ~ type + horizon +
                       re(random = ~1|inact),
                     family = GA(mu.link = "identity"),
                     data = meanscores)
summary.median_model <- summary(median_model)
summary.mean_model <- summary(mean_model)

saveRDS(median_model, 
        here("results", "add_model_gamma", "median_nmod4_plde.RDS"))
saveRDS(summary.median_model, 
        here("results", "add_model_gamma", "median_nmod4_plde_summary.RDS"))
saveRDS(mean_model, 
        here("results", "add_model_gamma", "mean_nmod4_plde.RDS"))
saveRDS(summary.mean_model, 
        here("results", "add_model_gamma", "mean_nmod4_plde_summary.RDS"))
