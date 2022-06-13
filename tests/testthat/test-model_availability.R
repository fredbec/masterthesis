library(here)
library(masterthesis)

source(here("code", "load_clean_data.R"))

here::i_am("tests/testthat/test-model_availability.R")

hub_avail <- hub_data

#check if models are still the same
expect_equal(sort(unique(hub_data$model)), 
             sort(unique(hub_avail$model))) #sort, otherwise fails bc of order


#check if availability was computed correctly
cvg_ensemble <- hub_avail |>
  filter(model == "EuroCOVIDhub-ensemble") |>
  select(availability) |>
  distinct()

#ensemble should have full availability
expect_equal(cvg_ensemble$availability, 1)


#check if anything at all about the core data was changed
hub_data_check <- hub_avail |> #"recover" ger_data from ger_cvg
  select(names(hub_data))

expect_true(all.equal(hub_data, hub_data_check,
                      ignore.row.order = TRUE))
#(it does have different order, but I guess that's fine?)