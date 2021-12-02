# Calculate excess births under various models

# Init ------------------------------------------------------------

library(tidyverse); library(yaml)

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
  observed_and_expected = 'tmp/expected_cv.rds',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  excess = 'tmp/excess.rds'
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

# load data on predicted counts
observed_and_expected <-
  readRDS(paths$input$observed_and_expected) %>%
  select(-model_para) %>%
  filter(cv_id == 0) %>%
  unnest(predictions) %>%
  filter(cv_sample == 'test')

# Calculate excess count statistics -------------------------------

excess <- list()

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
    model_id, region_lvl_2,
    obs_id, year, month, exposure, observed, starts_with('simulated')
  ) %>%
  nest(data = c(obs_id, year, month, exposure, observed, starts_with('simulated')))

excess$excess_measures <-
  excess$observed_and_expected %>%
  group_by(model_id, region_lvl_2) %>%
  group_modify(~{
    
    cat(.y[['model_id']], .y[['region_lvl_2']], '\n')
    
    # a time ordered data frame of observed and simulated counts
    # for a single region, sex, age group and model, starting early 2020
    # 1 row per observation
    X <- .x$data[[1]]
    
    # return data frame of row-wise quantiles over columns of X
    Rowquantiles <- function (X, prob, type = 4, na.rm = TRUE) {
      t(apply(X, 1, quantile, prob = prob, type = type, na.rm = na.rm))
    }
    
    # observed counts
    obs_t <- X$observed
    # cumulative observed counts (at end of time interval)
    obs_cum <- cumsum(obs_t)
    
    # simulated expected counts
    xpc_t_sim <- as.matrix(X[,excess$varnames_simulated])
    # simulated cumulative expected counts
    xpc_cum_sim <- apply(xpc_t_sim, 2, cumsum)
    
    # expected counts quantiles
    xpc_t <- Rowquantiles(xpc_t_sim, excess$quantiles1)
    colnames(xpc_t) <- paste0('xpc_t_', names(excess$quantiles1))
    # cumulative expected counts quantiles
    xpc_cum <- Rowquantiles(xpc_cum_sim, excess$quantiles1)
    colnames(xpc_cum) <- paste0('xpc_cum_', names(excess$quantiles1))
    
    # excess thresholds
    xtr_t <- Rowquantiles(xpc_t_sim, prob = excess$quantiles2)
    
    # excess counts type 1 quantiles
    xc1_t <- Rowquantiles(obs_t-xpc_t_sim, excess$quantiles1)
    colnames(xc1_t) <- paste0('xc1_t_', names(excess$quantiles1))
    # cumulative counts type 1 quantiles
    xc1_cum <- Rowquantiles(obs_cum-xpc_cum_sim, excess$quantiles1)
    colnames(xc1_cum) <- paste0('xc1_cum_', names(excess$quantiles1))
    
    # excess counts type 2 quantiles (counts above threshold)
    xc2_t <- obs_t-xtr_t
    xc2_t[xc2_t < 0] <- 0
    colnames(xc2_t) <- paste0('xc2_t_', names(excess$quantiles2))
    # cumulative excess counts type 2 quantiles
    xc2_cum <- apply(xc2_t, 2, cumsum)
    colnames(xc2_cum) <- paste0('xc2_cum_', names(excess$quantiles2))
    
    # simulated expected counts 0 adjusted
    xpc_t_sim_zad <- xpc_t_sim
    xpc_t_sim_zad[xpc_t_sim_zad == 0] <- 1
    # simulated cumulative expected counts 0 adjusted
    xpc_cum_sim_zad <- xpc_cum_sim
    xpc_cum_sim_zad[xpc_cum_sim_zad == 0] <- 1
    
    # P-score quantiles
    psc_t <- Rowquantiles((obs_t-xpc_t_sim)/xpc_t_sim_zad,
                            excess$quantiles1, type = 7)
    psc_t <- round(psc_t, digits = 3)
    colnames(psc_t) <- paste0('psc_t_', names(excess$quantiles1))
    # cumulative P-score quantiles
    psc_cum <- Rowquantiles((obs_cum-xpc_cum_sim)/xpc_cum_sim_zad,
                            excess$quantiles1, type = 7)
    psc_cum <- round(psc_cum, digits = 3)
    colnames(psc_cum) <- paste0('psc_cum_', names(excess$quantiles1))
    
    timeseries_of_measures <-
      cbind(
        X[,c('year', 'month', 'exposure')],
        obs_t, obs_cum,
        xpc_t, xpc_cum,
        xc1_t, xc1_cum,
        xc2_t, xc2_cum,
        psc_t, psc_cum
      )
    
    return(timeseries_of_measures)
    
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
  region = 'DE11',
  model_id = 'mav',
  timebase = 't'
)

excess$excess_measures_wide %>%
  filter(region_lvl_2 == ps$region) %>%
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
  geom_line(aes(y = q50)) +
  geom_point(aes(y = obs)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    limits = as.Date(c('2020-03-01', '2021-11-01'))
  ) +
  figspec$MyGGplotTheme(axis = '') +
  theme(panel.background = element_rect(fill = 'grey95', color = NA)) +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xpc', ps$timebase)
  )

# Plot xc1 --------------------------------------------------------

ps <- list(
  region = 'DE11',
  model_id = 'mav',
  timebase = 'cum'
)

excess$excess_measures_wide %>%
  filter(region_lvl_2 == ps$region) %>%
  mutate(date = MonthdateToDate(year, month)) %>%
  rename(
    q05 = paste0('xc1_', ps$timebase, '_q05_', ps$model_id),
    q25 = paste0('xc1_', ps$timebase, '_q25_', ps$model_id),
    q50 = paste0('xc1_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xc1_', ps$timebase, '_q75_', ps$model_id),
    q95 = paste0('xc1_', ps$timebase, '_q95_', ps$model_id)
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
    limits = as.Date(c('2020-03-01', '2021-11-01'))
  ) +
  figspec$MyGGplotTheme() +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xc1', ps$timebase)
  )

# Plot xc2 --------------------------------------------------------

ps <- list(
  region = 'DE11',
  model_id = 'mav',
  timebase = 'cum'
)

excess$excess_measures_wide %>%
  filter(region_lvl_2 == ps$region) %>%
  mutate(date = MonthdateToDate(year, month)) %>%
  rename(
    q50 = paste0('xc2_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xc2_', ps$timebase, '_q70_', ps$model_id),
    q90 = paste0('xc2_', ps$timebase, '_q90_', ps$model_id),
    q95 = paste0('xc2_', ps$timebase, '_q95_', ps$model_id)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q50, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_ribbon(
    aes(ymax = q90, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.3)
  ) +
  geom_ribbon(
    aes(ymax = q95, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.1)
  ) +
  geom_hline(yintercept = 0, color = 'grey20') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    limits = as.Date(c('2020-03-01', '2021-11-01'))
  ) +
  figspec$MyGGplotTheme() +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xc2', ps$timebase)
  )

# Plot psc --------------------------------------------------------

ps <- list(
  region = 'DE11',
  model_id = 'mav',
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

load(paths$input$input_data)

# merge with input data
excess$data_all_merge <-
  data_all %>%
  mutate(id = GenerateRowID(NUTS2code, Year, Month)) %>%
  left_join(excess$excess_measures_for_export, by = 'id')
saveRDS(excess$data_all_merge, paths$output$excess)
