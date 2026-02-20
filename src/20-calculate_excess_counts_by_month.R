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
  excess = 'out/20-excess_by_month.rds'
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

excess <- list()

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

# names of columns for expected/simulated counts
excess$varnames_simulated <-
  grep('simulated', colnames(observed_and_expected), value = TRUE)
# define quantiles
excess$quantiles1 <- c('q05' = 0.05, 'q25' = 0.25, 'q50' = 0.5, 'q75' = 0.75, 'q95' = 0.95)
excess$quantiles2 <- c('q50' = 0.5, 'q70' = 0.70, 'q90' = 0.9, 'q95' = 0.95, 'q99' = 0.99)

# nest by model id and stratum
excess$observed_and_expected <-
  observed_and_expected %>%
  select(
    model_id, region_lvl_1, region_lvl_2,
    obs_id, year, month, exposure, observed, starts_with('simulated')
  ) %>%
  nest(data = c(obs_id, year, month, exposure, observed, starts_with('simulated')))

# calculate excess statistics
excess$excess_measures <-
  excess$observed_and_expected %>%
  group_by(model_id, region_lvl_1, region_lvl_2) %>%
  group_modify(~{
    cat(.y[['model_id']], .y[['region_lvl_2']], '\n')
    GetExcessStatisticsByYearMonth(.x$data[[1]], excess$varnames_simulated, excess$quantiles1, excess$quantiles2)
  }) %>%
  ungroup()

# each measure*model in its own column
excess$excess_measures_wide <-
  excess$excess_measures %>%
  ungroup() %>%
  pivot_wider(
    names_from = model_id,
    values_from = c(xpc_t_q05:last_col()),
    names_glue = '{.value}_{model_id}'
  ) %>%
  # add row id
  mutate(
   id = GenerateRowID(region_lvl_2, year, month)
  ) %>%
  # ensure lowercase varnames
  rename_with(tolower)

# reshuffle columns
excess$excess_measures_for_export <-
  excess$excess_measures_wide[, unlist(c(
  'id', 'exposure',
  # observed counts
  'obs_t', 'obs_cum',
  # excess statistics
  sapply(tolower(cnst$models_to_include), function (mod) {
    c(
      grep(paste0('xpc_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('xc1_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('xc2_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('psc_.+_', mod), names(excess$excess_measures_wide), value = TRUE)          
    )
  })
))]

# Plot XPC --------------------------------------------------------

ps <- list(
  region = 'ESP',
  model_id = 'bms',
  timebase = 't'
)

excess$excess_measures_wide %>%
  filter(region_lvl_1 == ps$region) %>%
  mutate(date = MonthdateToDate(year, month)) %>%
  rename(
    q05 = paste0('xpc_', ps$timebase, '_q05_', ps$model_id),
    q25 = paste0('xpc_', ps$timebase, '_q25_', ps$model_id),
    q50 = paste0('xpc_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xpc_', ps$timebase, '_q75_', ps$model_id),
    q95 = paste0('xpc_', ps$timebase, '_q95_', ps$model_id),
    obs = paste0('obs_', ps$timebase)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = q25),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_point(aes(y = obs), color = 'black', size = 0.5) +
  geom_line(aes(y = q50)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  facet_wrap(~region_lvl_2, scales = 'free_y') +
  scale_x_date(
  date_breaks = '1 year', date_labels = '%Y'
  ) +
  figspec$MyGGplotTheme(axis = '') +
  theme(panel.background = element_rect(fill = 'grey95', color = NA)) +
  labs(
    title = paste('Expected vs. observed monthly births since November 2021', ps$region),
    y = 'Expected vs. observed'
  )

# Plot psc --------------------------------------------------------

ps <- list(
  region = 'DE11',
  model_id = 'bms',
  timebase = 't'
)

excess$excess_measures_wide %>%
  filter(region_lvl_2 == ps$region) %>%
  mutate(date = MonthdateToDate(year, month)) %>%
  rename(
    q05 = paste0('psc_', ps$timebase, '_q05_', ps$model_id),
    q25 = paste0('psc_', ps$timebase, '_q25_', ps$model_id),
    q50 = paste0('psc_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('psc_', ps$timebase, '_q75_', ps$model_id),
    q95 = paste0('psc_', ps$timebase, '_q95_', ps$model_id)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = q25),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_line(aes(y = q50)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    minor_breaks = '1 month',
    limits = as.Date(c('2020-03-01', '2021-11-01'))
  ) +
  scale_y_continuous(labels = scales::percent) +
  figspec$MyGGplotTheme() +
  theme(
    panel.grid.major.x = element_line(color = 'grey50'),
    panel.grid.minor.x = element_line(color = 'grey50')
  ) +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('psc', ps$timebase)
  )

# Export ----------------------------------------------------------

# merge with input data
excess$data_all_merge <-
  data_all %>%
  mutate(id = GenerateRowID(NUTS2code, Year, Month)) %>%
  left_join(excess$excess_measures_for_export, by = 'id')
saveRDS(excess$data_all_merge, paths$output$excess)
