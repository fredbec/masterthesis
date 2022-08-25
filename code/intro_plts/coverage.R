library(here)
library(masterthesis)
library(scoringutils)

source(here("code", "load_clean_data.R"))
source(here("specs", "specs.R"))

plot_models <- c("Karlen-pypm", "MUNI-ARIMA", "MOCOS-agent1")
plot_ranges <- c(seq(10, 90, by = 10), 95, 98) 

#plot_models <- hub_data |>
#  filter(availability > 0.5) |>
#  select(model) |>
#  pull()

###STILL NEED TO ADD ANOTHER PLOT; OTHERWISE TOO EMPTY

cvg_scores <- hub_data |>
  #filter(availability > 0.5) |>
  select(specs$su_cols) |>
  filter(model %in% plot_models) |>
  score() |>
  add_coverage(ranges = plot_ranges,
               by = "model") |>
  summarise_scores(by = "model") |>
  select(all_of(c("model", paste0("coverage_", plot_ranges)))) |>
  mutate(coverage_0 = 0,
         coverage_100 = 1) |>
  melt(id.vars = c("model")) |>
  mutate(nominal = rep(c(plot_ranges,0,100), each = length(plot_models))/100) 


 
pdf(here("plots", "theo_coverage.pdf"), 
    height = 8, width = 8)
ggplot(cvg_scores, aes(x = nominal, y = value,
                       color = model))  +
  geom_line() +
  #geom_point() +
  #geom_abline(intercept = 0, slope = 1) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               colour = "black",lty = "dashed") +
  scale_color_discrete(name="") + 
  xlab("Nominal coverage") +
  ylab("Empirical interval coverage") + 
  theme_masterthesis() 
dev.off()

