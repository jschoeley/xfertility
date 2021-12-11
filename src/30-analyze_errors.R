# Determine error and bias of various models of expected counts

# Init ------------------------------------------------------------

library(glue)
library(tidyverse)
library(patchwork)
library(yaml)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  glob = 'src/00-global_objects.R',
  fitted_models = 'tmp/expected_cv.rds',
  config = 'src/config.yaml',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  out = 'out'
)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  model_metadata = read_csv(paths$input$model_metadata)
  models_to_include = model_metadata[model_metadata$include==1,][['code']]
  config = read_yaml(paths$input$config)  
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# global functions and constants
source(paths$input$glob)

# Load data -------------------------------------------------------

# load data for cross validation
dat$fitted_models <- readRDS(paths$input$fitted_models)
# fitted cross-validation series, only test data
dat$cv_test <-
  dat$fitted_models %>%
  filter(
    cv_id != 0,
#    model_id %in% cnst$models_to_include,
    error_while_fit == FALSE
  ) %>%
  unnest(predictions) %>%
  filter(cv_sample == 'test') %>%
  select(-contains('simulated'))

# Calculate errors on multiple aggregation levels -----------------

# residuals and error measures by month
dat$residual_month <-
  CountResiduals(
    dat$cv_test,
    weeks_since_test_start, observed, predicted, cv_id,
    model_id,
    region_lvl_2
  )

# per-region w(m)ape of predicted counts weighted by date
dat$residual_month$wmape <-
  dat$residual_month$residual_raw %>%
  group_by(model_id, region_lvl_2) %>%
  summarise(
    wmape_count = sum(abs(resid_e))/sum(observed),
    wmpe_count = sum(resid_e)/sum(observed),
    wmape_cum = sum(abs(resid_e_cum[
      weeks_since_test_start == (cnst$config$forecast$forecast_n_months*4-1)
    ]))/sum(observed_cum[
      weeks_since_test_start == (cnst$config$forecast$forecast_n_months*4-1)]),
    wmpe_cum = sum(resid_e_cum[
      weeks_since_test_start == (cnst$config$forecast$forecast_n_months*4-1)
    ])/sum(observed_cum[
      weeks_since_test_start == (cnst$config$forecast$forecast_n_months*4-1)])
  ) %>%
  ungroup()
dat$residual_month$wmape_summary <-
  dat$residual_month$wmape %>%
  group_by(model_id) %>%
  summarise(
    wmape_count_q50 = median(wmape_count, na.rm = TRUE),
    wmape_count_q25 = quantile(wmape_count, na.rm = TRUE, p = 0.25),
    wmape_count_q75 = quantile(wmape_count, na.rm = TRUE, p = 0.75),
    wmpe_count_q50 = median(wmpe_count, na.rm = TRUE),
    wmpe_count_q25 = quantile(wmpe_count, na.rm = TRUE, p = 0.25),
    wmpe_count_q75 = quantile(wmpe_count, na.rm = TRUE, p = 0.75),
    wmape_cum_q50 = median(wmape_cum, na.rm = TRUE),
    wmape_cum_q25 = quantile(wmape_cum, na.rm = TRUE, p = 0.25),
    wmape_cum_q75 = quantile(wmape_cum, na.rm = TRUE, p = 0.75),
    wmpe_cum_q50 = median(wmpe_cum, na.rm = TRUE),
    wmpe_cum_q25 = quantile(wmpe_cum, na.rm = TRUE, p = 0.25),
    wmpe_cum_q75 = quantile(wmpe_cum, na.rm = TRUE, p = 0.75),
  )

# Figure errorbias ------------------------------------------------

PlotErrorsAndBias <- function (df_errors, error_measure, bias_measure, xlab) {
  
  require(tidyverse)
  
  Format <- function (x) { formatC(x, format = 'f', digits = 1) }
  
  observed_errors <-
    df_errors %>%
    select(
      model = model_id,
      error_measure = {{error_measure}},
      bias_measure = {{bias_measure}}
    )

  # summarise errors over countries and possibly strata and weeks
  summarised_errors <-
    observed_errors %>%
    mutate(
      error_measure = error_measure*100,
      bias_measure = bias_measure*100
    ) %>%
    group_by(model) %>%
    summarise(
      error_qlo = quantile(error_measure, p = 0.25, na.rm = TRUE),
      error_qmd = quantile(error_measure, p = 0.5, na.rm = TRUE),
      error_qhi = quantile(error_measure, p = 0.75, na.rm = TRUE),
      bias_qlo = quantile(bias_measure, p = 0.25, na.rm = TRUE),
      bias_qmd = quantile(bias_measure, p = 0.5, na.rm = TRUE),
      bias_qhi = quantile(bias_measure, p = 0.75, na.rm = TRUE)
    ) %>%
    left_join(cnst$model_metadata, c('model' = 'code')) %>%
    mutate(
      model =
        fct_reorder(model, -error_qmd)
    )
  
  ynudge <- 0.2
  sizelarge <- 1
  sizesmall <- 0.6
  textsize <- 2
  sizeribbon <- 7
  
  fig <-
    summarised_errors %>%
    ggplot(aes(y = model, yend = model)) +
    # indicate rows
    geom_segment(
      aes(color = highlight),
      x = -Inf, xend = Inf, size = sizeribbon
    ) +
    geom_vline(xintercept = 0, color = 'grey50', size = 1.5) +
    # plot errors
    geom_segment(
      aes(x = error_qlo, xend = error_qhi),
      position = position_nudge(y = ynudge),
      size = sizesmall
    ) +
    geom_label(
      aes(x = error_qmd, label = Format(error_qmd)),
      position = position_nudge(y = ynudge),
      label.r = unit(0, 'pt'), size = textsize, fontface = 'italic',
      label.padding = unit(1, 'pt')
    ) +
    geom_text(
      aes(x = error_qlo-0.3, label = Format(error_qlo)),
      position = position_nudge(y = ynudge),
      hjust = 'right', size = textsize, fontface = 'italic'
    ) +
    geom_text(
      aes(x = error_qhi+0.3, label = Format(error_qhi)),
      position = position_nudge(y = ynudge),
      hjust = 'left', size = textsize, fontface = 'italic'
    ) +
    # plot bias
    geom_segment(
      aes(x = bias_qlo, xend = bias_qhi),
      position = position_nudge(y = -ynudge),
      size = sizelarge
    ) +
    geom_label(
      aes(x = bias_qmd, label = Format(bias_qmd)),
      position = position_nudge(y = -ynudge),
      label.r = unit(0, 'pt'), size = textsize, fontface = 'bold',
      label.padding = unit(1, 'pt')
    ) +
    geom_text(
      aes(x = bias_qlo-0.3, label = Format(bias_qlo)),
      position = position_nudge(y = -ynudge),
      hjust = 'right', size = textsize
    ) +
    geom_text(
      aes(x = bias_qhi+0.3, label = Format(bias_qhi)),
      position = position_nudge(y = -ynudge),
      hjust = 'left', size = textsize
    ) +
    # misc
    figspec$MyGGplotTheme(show_legend = FALSE) +
    scale_x_continuous(breaks = seq(-10, 10, 5)) +
    scale_color_brewer(type = 'qual', palette = 5) +
    labs(y = NULL, x = xlab) +
    coord_cartesian(clip = 'off', xlim = c(-10,10))
  
  list(errors = summarised_errors, fig = fig)
  
}

# error & bias of annual counts on country level
fig$errorbias_a <-
  dat$residual_month$wmape %>%
  PlotErrorsAndBias(
    error_measure = wmape_cum,
    bias_measure = wmpe_cum,
    xlab = NULL
  )

# error & bias of monthly counts on country level
fig$errorbias_b <-
  dat$residual_month$wmape %>%
  PlotErrorsAndBias(
    error_measure = wmape_count,
    bias_measure = wmpe_count,
    xlab = 'MPE/MAPE'
  )

# assemble multi-panel figure 3
fig$errorbias <-
  fig$errorbias_a$fig +
  labs(subtitle = 'a. Total births by NUTS-2', y = 'Model') +
  fig$errorbias_b$fig +
  labs(subtitle = 'b. Total monthly births by NUTS-2') +
  plot_layout(ncol = 2, byrow = TRUE)
fig$errorbias

ExportFigure(
  fig$errorbias, path = paths$output$out, filename = 'errorbias',
  add_date = FALSE,
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width
)
