models <- c("LANL-GrowthRate", "EuroCOVIDhub-baseline", "itwm-dSEIR", "EuroCOVIDhub-ensemble", "MUNI-ARIMA")
horizons <- 1:3

fcdate <- "2021-05-17"
target <- "Deaths"
loc <- "DE"

mydat <- hub_data |>
  filter(model %in% models,
         horizon %in% horizons,
         target_type == target,
         forecast_date == fcdate,
         location == loc)


ggplot(mydat, aes(y = quantile, x = prediction, color = as.factor(horizon))) +
  geom_point() +
  facet_wrap( ~ model)


hub_data |>
  filter(location %in% c("DE", "CZ", "PL", "GB"), horizon == 1, 
         forecast_date %in% seq.Date(as.Date("2021-05-03"), 
                                     as.Date("2021-10-04"), by = 7)) |>
  group_by(target_type, location) |>
  summarise(truval = mean(true_value))


#identify series where never more than xx change