library(here)

source(here("code", "load_clean_data.R"))
source(here("code", "fun_model_coverage.R"))

here::i_am("code/tests/test_fun_model_coverage.R")


ger_data <- hub_data |>
  filter(location == "DE")

ger_cvg <- model_coverage(ger_data, "DE")


#check if models are still the same
expect_equal(sort(unique(ger_data$model)), 
             sort(unique(ger_cvg$model))) #sort, otherwise fails bc of order


#check if coverage was computed correctly
cvg_ensemble <- ger_cvg |>
  filter(model == "EuroCOVIDhub-ensemble") |>
  select(coverage) |>
  distinct()

expect_equal(cvg_ensemble$coverage, 1)


#check if anything at all about the core data was changed
ger_data_check <- ger_cvg |> #"recover" ger_data from ger_cvg
  select(names(ger_data))

expect_true(all.equal(ger_data, ger_data_check,
                      ignore.row.order = TRUE))
#(it does have different order, but I guess that's fine?)