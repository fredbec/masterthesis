library(here)
library(masterthesis)
library(scoringutils)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(dplyr)
library(magrittr)
source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

avail_threshold <- specs$model_type_avail_threshold

#######Exclude models in "other"########
pw_comp_wo_other <- hub_data |>
  filter(!model %in% c("other", "ensemble"),
         availability > avail_threshold) |>
  pw_comp_by_model_type(specs$su_cols) #|>

plot_pw_comp_wo_other <- pw_comp_wo_other |>
  plot_pairwise_comparison() +
  facet_wrap(~horizon + target_type, ncol = 2)

pdf(here("plots", "pw_comp_model_type_wo_agent.pdf"), 
    height = 11.69, width = 8.27)
plot_pw_comp_wo_other
dev.off()



#####Split up models in "other" (agent-based vs expert)
pw_comp_with_other <- hub_data |>
  filter(availability > avail_threshold) |>
  mutate(model_type = 
           ifelse(model %in% c("MOCOS-agent1", "ICM-agentModel"),
                  "agent-based", model_type)) |>
  mutate(model_type = 
           ifelse(grepl("*Expert", model),
                  "expert judgment", model_type)) |>
  pw_comp_by_model_type(specs$su_cols)

plot_pw_comp_with_other <- pw_comp_with_other |>
  plot_pairwise_comparison() +
  theme(
    panel.spacing = unit(1.5, "lines")
  ) + 
  facet_grid(horizon ~ target_type,
             labeller = as_labeller(c(specs$plot_horizon_label,
                                      specs$plot_target_label)))

pdf(here("plots", "pw_comp_model_type_with_other.pdf"), 
    height = 11.69, width = 8.27)
plot_pw_comp_with_other
dev.off()



######################Split up by periods#####################
hub_data_by_period <- hub_data |>
  left_join(specs$period_cat_dt, by = c("forecast_date")) |>
  filter(!model_type == "other") |>
  arrange(location, target_type, forecast_date, horizon, model, quantile) |>
  split(by = "period_cat")


#####By horizon######
#pairwise comparisons by horizon
pw_comp_by_period_and_hor <- lapply(
  hub_data_by_period, function(split_dat) 
    pw_comp_by_model_type(split_dat, specs$su_cols)) |>
  rbindlist(idcol = "period") |>
  relocate(period, .after = last_col())

#pairwise comparisons across horizon
pw_comp_by_period <- lapply(
  hub_data_by_period, function(split_dat) 
    pw_comp_by_model_type(split_dat, specs$su_cols,
                          #change end_res to across horizon
                          end_res = c("model", "target_type")))|> 
  rbindlist(idcol = "period") |>
  relocate(period, .after = last_col()) |>
  mutate(horizon = "average") |>
  #attach data with horizon stratification
  rbind(pw_comp_by_period_and_hor) |>
  #only interested in comparison against baseline
  filter(model != "baseline",
         compare_against == "baseline")

  
pdf(here("plots", "pw_comp_model_types_across_periods.pdf"),
    height = 11.69, width = 8.27)
plot_model_type_across_time(pw_comp_by_period)
dev.off()



####By location#####
#pairwise comparisons by horizon
pw_comp_by_period_and_loc <- lapply(
  hub_data_by_period, function(split_dat) 
    pw_comp_by_model_type(split_dat, specs$su_cols,
                          end_res = c("model", "location", "target_type"))) |>
  rbindlist(idcol = "period") |>
  relocate(period, .after = last_col()) |>
  #only interested in comparison against baseline
  filter(model != "baseline",
         compare_against == "baseline")

pdf(here("plots", "pw_comp_model_types_across_periods_and_loc.pdf"),
    height = 11.69, width = 8.27)
plot_model_type_across_time_and_loc(pw_comp_by_period_and_loc)
dev.off()



####################Other plots####################
#################WIS decomposition###########
decomp_scores <- compute_decomp_scores(hub_data, 
                                       adjust_avail = TRUE)

pdf(here("plots", "wis_decomp_model_types.pdf"),
    height = 7, width = 12)
wis_decomp_model_type(decomp_scores)
dev.off()


##############Overall assessment plot###########
model_type_scores <- overall_assessment_model_types(hub_data, 
                                                    adjust_avail = TRUE)

pdf(here("plots", "overall_assessment_model_types.pdf"),
    height = 9, width = 10)
overall_assessment_plot(model_type_scores, decomp_scores)
dev.off()
