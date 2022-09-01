library(here)
library(dplyr)

name_w_median <- "weighted median"
name_median <- "unw. median"
name_w_mean <- "weighted mean"
name_mean <- "unw. mean"

rel_scores_best_perform <- fread(here("score_data", "rel_scores_best_perform.csv"))
bp_chosen_mods <- fread(here("ensembles", "weights", "best_performers_incl_mods.csv"))


#aggregate scores, stratified by location, target type, ensemble type
#for nmod %in% c(5,10)
agg_scores <- rel_scores_best_perform |>
  filter(nmod %in% c(5,10),
         model %in% c("median_ensemble",
                      "weighted.median_ensemble",
                      "mean_ensemble", 
                      "weighted.mean_ensemble")) |>
  group_by(model, location, target_type, nmod) |>
  summarise(tmean = mean(target_val),
            cmean = mean(current_val)) |>
  mutate(rel_wis = tmean/cmean) |>
  select(model, location, target_type, nmod, rel_wis) |> 
  mutate(rel_wis = round(rel_wis, 2)) |>
  mutate(model = ifelse(model == "median_ensemble", name_median, model),
         model = ifelse(model == "weighted.median_ensemble", name_w_median, model),
         model = ifelse(model == "mean_ensemble", name_mean, model),
         model = ifelse(model == "weighted.mean_ensemble", name_w_mean, model)) |>
  mutate(model = factor(model, levels = c(name_median,
                                          name_w_median,
                                          name_mean,
                                          name_w_mean),
                        ordered = TRUE)) |>
  ungroup() |>
  setDT() |>
  dcast(model + nmod  + target_type ~ location , value.var = "rel_wis") |>
  arrange(target_type, nmod, model)
   
cat(knitr::kable(agg_scores, format = 'latex'), file = 'thesis/tables/best_perform_results.tex') 


#most chosen models for nmod = 5, for location and target type
chosen_models <- bp_chosen_mods |>
  filter(nmod == 5) |> 
  group_by(target_type, location, model) |> 
  summarise(summ = n()) |> 
  group_by(location, target_type) |> 
  slice_max(n = 3, order_by = summ) |>
  mutate(modeln = paste0(model, " (", summ, ")")) |>
  setDT() |>
  select(-c(model, summ)) |>
  ungroup() |>
  split(by = c("location", "target_type")) |>
  lapply(function(x) data.table(models = paste(x$modeln, collapse = ", "))) |>
  rbindlist(idcol = TRUE) |>
  tidyr::separate(.id, into = c("location", "target_type"), sep = "[.]") |>
  arrange(target_type)


cat(knitr::kable(chosen_models, format = 'latex'), file = 'thesis/tables/best_perform_models.tex') 




