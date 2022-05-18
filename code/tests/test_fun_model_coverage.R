library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_model_coverage.R"))

here::i_am("code/tests/test_fun_model_coverage.R")


hub_data <- hub_data |>
  filter(location %in% c("DE", "FR"))

hub_cvg <- model_coverage(hub_data)


#check if models are still the same
expect_equal(sort(unique(hub_data$model)), 
             sort(unique(hub_cvg$model))) #sort, otherwise fails bc of order


#check if coverage was computed correctly
cvg_ensemble <- hub_cvg |>
  filter(model == "EuroCOVIDhub-ensemble") |>
  select(coverage) |>
  distinct()

#ensemble should have full coverage
expect_equal(cvg_ensemble$coverage, 1)


#check if anything at all about the core data was changed
hub_data_check <- hub_cvg |> #"recover" ger_data from ger_cvg
  select(names(hub_data))

expect_true(all.equal(hub_data, hub_data_check,
                      ignore.row.order = TRUE))
#(it does have different order, but I guess that's fine?)