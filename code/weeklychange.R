hub_data |>
  filter(horizon == 1) |>
  select(target_type, location, true_value, forecast_date) |>
  distinct() |>
  group_by(target_type, location) |>
  arrange(forecast_date, .by_group = TRUE) |>
  mutate(trueval_diff = c(0,diff(true_value))) |>
  mutate(oldval = true_value - trueval_diff) |>
  mutate(change = trueval_diff/oldval) |>
  View()
  mutate(truevaldiff  = (c(0, diff(true_value))/
                           c(1, true_value-diff(true_value))))
  
  
  

plot_locs_per_model <- function(data) {
  data |>
    filter(!model_type %in% c("other/semi", "ensemble", "baseline")) |>
    group_by(model_type, forecast_date, location) |>
    mutate(n = length(unique(model))) |>
    ggplot(aes(y = reorder(model_type, n), x = as.Date(forecast_date), fill = n)) + 
    geom_tile() + 
    facet_wrap(location ~ target_type, nrow = 5, ncol = 2) + 
    labs(y = "Location", x = "Forecast date")
} 

pdf("model_type_per_loc.pdf", height = 12, width = 8)
plot_locs_per_model(hub_data)
dev.off()
