# Calculate excess births under various models

# Init ------------------------------------------------------------

library(tidyverse); library(yaml)
library(qs)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  input_data = 'dat/NUTS2_data_fertility_rate_all_with_covid.Rdat',
  config = 'src/config.yaml',
  # global objects
  glob = 'src/00-global_objects.R',
  # observed and expected counts 2020 and later
  observed_and_expected = 'out/14-expected_with_bms_sim.qs',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  excess_by_period_region = 'out/21-excess_by_period_region.rds',
  excess_by_period_country = 'out/21-excess_by_period_country.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  model_metadata = read_csv(paths$input$model_metadata)
  models_to_include = model_metadata[model_metadata$include==1,][['code']]
})

# global functions and constants
source(paths$input$glob)

# Load data -------------------------------------------------------

load(paths$input$input_data)

# load data on predicted counts
observed_and_expected <-
  qread(paths$input$observed_and_expected) %>%
  select(-model_para) %>%
  filter(cv_id == 0) %>%
  unnest(predictions) %>%
  filter(
    cv_sample == 'test',
    model_id == 'BMS'
  )

# Calculate excess count statistics -------------------------------

# MONTHLY
# PERIOD I : [11/2020, 12/2020, 1/2021] FIRST WAVE CONCEPTIONS
#   conceptions: 2-4 2020, covid: Start pandemic 1/2020 to April 4/2020
# PERIOD II: [2/2021, 3/2021] FIRST RE-OPENING
#   conceptions: 5/6 2020, covid: May and June 2020 cases
# PERIOD III: [4/2021, 5/2021, 6/2021] SUMMER 21
#   conceptions: 7-9 2020, covid: 7-9 2020
# PERIOD IV: [7/2021, 8/2021, 9/2021, 10/2021] SECOND WAVE CONEPTIONS
#   conceptions: 10 2020 and 1/2021, covid: 10/2020-1/2021
# PERIOD V: [11/2021, 12/2021, 1/2022] THIRD WAVE CONCEPTIONS
#   conception: 2-4 2021 conception, covid: Feb April 2021
# PERIOD I+II+III
# PERIOD IV+V
# PERIOD I+II+III+IV+V
excess <- within(list(), {
  period_I = list(
    name = 'I',
    dates = c('2020-11', '2020-12', '2021-01')
  )
  period_II = list(
    name = 'II',
    dates = c('2021-02', '2021-03')
  )
  period_III = list(
    name = 'III',
    dates = c('2021-04', '2021-05', '2021-06')
  )
  period_IV = list(
    name = 'IV',
    dates = c('2021-07', '2021-08', '2021-09', '2021-10')
  )
  period_V = list(
    name = 'V',
    dates = c('2021-11', '2021-12', '2022-01')
  )
  periods_I_II_III = list(
    name = 'I+II+III',
    dates = c(period_I$dates, period_II$dates, period_III$dates)
  )
  periods_IV_V = list(
    name = 'IV+V',
    dates = c(period_IV$dates, period_V$dates)
  )
  periods_all = list(
    name = 'Total',
    dates = c(period_I$dates, period_II$dates, period_III$dates,
              period_IV$dates, period_V$dates)
  )
})

# names of columns for expected/simulated counts
varnames_simulated <- 
  grep('simulated', colnames(observed_and_expected), value = TRUE)
# define quantiles
quantiles1 <- c('q05' = 0.05, 'q25' = 0.25, 'q50' = 0.5, 'q75' = 0.75, 'q95' = 0.95)
quantiles2 <- c('q50' = 0.5, 'q70' = 0.70, 'q90' = 0.9, 'q95' = 0.95, 'q99' = 0.99)

excess_by_period_region <- lapply(excess, function (period) {

  # aggregation step
  aggregated <-
    observed_and_expected |>
    select(
      model_id, region_lvl_1, region_lvl_2,
      obs_id, year, month, exposure, observed, starts_with('simulated')
    ) |>
    mutate(yearmonth = paste(year, formatC(month, flag = '0', format = 'd', digits = 1), sep = '-')) |>
    filter(yearmonth %in% period$dates) |>
    group_by(model_id, region_lvl_1, region_lvl_2) |>
    summarise(across(c(exposure, observed, starts_with('simulated')), sum)) |>
    mutate(period = period$name) |>
    ungroup()
  
  # nest by model id and stratum
  nested <-
    aggregated |>
    nest(data = c(period, exposure, observed, starts_with('simulated')))
  
  # calculate excess statistics  
  nested |>
    group_by(model_id, region_lvl_2) |>
    group_modify(~{
      cat(period$name, .y[['model_id']], .y[['region_lvl_2']], '\n')
      GetExcessStatisticsByPeriod(.x$data[[1]], varnames_simulated, quantiles1, quantiles2)
    }) |>
    ungroup()
  
})

excess_by_period_country <- lapply(excess, function (period) {
  
  # aggregation step
  aggregated <-
    observed_and_expected |>
    select(
      model_id, region_lvl_1,
      obs_id, year, month, exposure, observed, starts_with('simulated')
    ) |>
    mutate(
      yearmonth =
        paste(year, formatC(month, flag = '0', format = 'd', digits = 1),
              sep = '-')
    ) |>
    filter(yearmonth %in% period$dates) |>
    group_by(model_id, region_lvl_1) |>
    summarise(across(c(exposure, observed, starts_with('simulated')), sum)) |>
    mutate(period = period$name) |>
    ungroup()
  
  # nest by model id and stratum
  nested <-
    aggregated |>
    nest(data = c(period, exposure, observed, starts_with('simulated')))
  
  # calculate excess statistics  
  nested |>
    group_by(model_id, region_lvl_1) |>
    group_modify(~{
      cat(period$name, .y[['model_id']], .y[['region_lvl_1']], '\n')
      GetExcessStatisticsByPeriod(.x$data[[1]], varnames_simulated, quantiles1, quantiles2)
    }) |>
    ungroup()
  
})

# Format output ---------------------------------------------------

# each measure*model in its own column
excess_by_period_region <-
  bind_rows(excess_by_period_region) |>
  ungroup() %>%
  pivot_wider(
    names_from = model_id,
    values_from = c(xpc_t_q05:last_col()),
    names_glue = '{.value}_{model_id}'
  ) %>%
  # ensure lowercase varnames
  rename_with(tolower)

# each measure*model in its own column
excess_by_period_country <-
  bind_rows(excess_by_period_country) |>
  ungroup() %>%
  pivot_wider(
    names_from = model_id,
    values_from = c(xpc_t_q05:last_col()),
    names_glue = '{.value}_{model_id}'
  ) %>%
  # ensure lowercase varnames
  rename_with(tolower)


# Export ----------------------------------------------------------

saveRDS(excess_by_period_region, paths$output$excess_by_period_region)
saveRDS(excess_by_period_country, paths$output$excess_by_period_country)
