# Prepare data for model fitting and cross-validation
#
# Find source data at N drive, NUTS2_data_fertility_rate_all.Rdat
#
# ## id variables
#
# - cv_id:
#   identifies cross-validation series;
#   id 0 contains the complete series
# - obs_id:
#   identifies rows across CV series; 
#   pattern: <region_iso, sex, age_start, iso_year, iso_week>
# - stratum_id:
#   identifies unique sex and age combinations
#
# ## strata
#
# - region_iso:
#   ISO 3166-1 alpha-2 country code when region is nation state;
#   ISO 3166-2 region code when region is subdivision of nation state
# - iso_year:
#   year as defined in ISO 8601 week date system
# - iso_week:
#   week as defined in ISO 8601 week date system [0, 52];
#   leap-weeks 53 dropped
#
# ## observations
#
# - deaths_observed:
#   number of deaths from any cause
# - population:
#   population count (only observed in week containing January 1st)
# - personweeks:
#   person-weeks of exposure
#
# ## additional calendar variables
#
# - date:
#   starting date of epi-week
# - origin_date:
#   starting date of cv series
# - origin_weeks:
#   completed weeks since start of cv series
# - origin_date_test:
#   starting date of test data
# - weeks_since_test_start:
#   weeks since start of test
#
# ## additional region information
# 
# - region_name:
#   natural name of region
# - region_level:
#   0 for nation state / country level, 1 for subdivision
# - country_iso:
#   ISO 3166-1 alpha-2 country code
# - country_name:
#   natural name of country
# - hemisphere:
#   (n)orth or (s)outh
# - continent:
#   continent of region
#
# ## flags
#
# - cv_sample:
#   does this data point belong to the CV id's 'training' or 'test' set?
# - cv_full_series:
#   are complete 5 fold CV series available for this country?
#   'TRUE' or 'FALSE'

# Init ------------------------------------------------------------

library(tidyverse); library(lubridate); library(yaml)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  config = 'src/config.yaml',
  input_data = 'dat/NUTS2_data_fertility_rate_all_with_covid.Rdat',
  global_constants = 'src/00-global_objects.R'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  data_cv = 'tmp/data_cv.rds'
)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  config = read_yaml(paths$input$config)
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# global functions and constants
source(paths$input$global_constants)

# Load data -------------------------------------------------------

# time series of monthly birth counts by region plus covariates
load(paths$input$input_data)

# Add time related variables --------------------------------------

dat$monthly_births <-
  data_all %>%
  as_tibble() %>%
  select(
    nuts1 = PopName, nuts2 = NUTS2code, year = Year, month = Month,
    observed_births = Births, personmonths = Population
  ) %>%
  arrange(nuts2, year, month) %>%
  mutate(
    # row id
    obs_id = GenerateRowID(region_nuts2 = nuts2, year, month),
    # date at start of month
    date = MonthdateToDate(year, month),
    # monthly factor
    month_fac = as.factor(month) %>% fct_relevel('1')
  ) %>%
  # months since start of series
  group_by(nuts2) %>%
  mutate(
    weeks_since_start = WeeksSinceOrigin(date, min(date))
  )

# Prepare cross-validation data sets ------------------------------

# define cross-validation series
# training: [training_start, test_start)
# test: [test_start, test_end]
dat$cv_selection <-
  tibble(
    cv_id = 1:5,
    training_start =
      MonthdateToDate(
        2007+0:4,
        month = 1
      ),
    test_start =
      MonthdateToDate(
        2012+0:4,
        month = cnst$config$forecast$start_of_test_month
      ),
    test_end = 
      test_start + months(cnst$config$forecast$forecast_n_months)
  )
# add the complete data series as cv_id 0
dat$total_selection <-
  tibble(
    cv_id = 0,
    training_start =
      MonthdateToDate(
        2007,
        month = 1
      ),
    test_start =
      MonthdateToDate(
        2020,
        month = cnst$config$forecast$start_of_test_month
      ),
    test_end = 
      test_start + months(cnst$config$forecast$forecast_n_months)
  )

dat$selection <-
  bind_rows(
    dat$total_selection, dat$cv_selection
  )

# countries with complete data for CV split
# complete means no missings in any of the "core" variables, i.e.
# births & personmonths
dat$country_selection <-
  dat$monthly_births %>%
  drop_na(observed_births, personmonths) %>%
  group_by(nuts2) %>%
  summarise(
    min_year = min(year)
  ) %>%
  filter(min_year <= 2007) %>%
  pull(nuts2)

# test-training data
dat$monthly_births_cv <-
  dat$selection %>%
  group_by(cv_id) %>%
  group_modify(~{
    
    # retrieve training data
    training <-
      filter(dat$monthly_births, date >= .x$training_start, date < .x$test_start) %>%
      mutate(cv_sample = 'training')
    # retrieve test data
    test <-
      filter(dat$monthly_births, date >= .x$test_start, date <= .x$test_end) %>%
      mutate(cv_sample = 'test')
    
    bind_rows(training, test) %>%
      # add date of series origin, weeks since origin and
      # date of test data start
      mutate(
        origin_date = .x$training_start,
        origin_weeks = WeeksSinceOrigin(date, .x$training_start),
        origin_date_test = .x$test_start,
        weeks_since_test_start = WeeksSinceOrigin(date, .x$test_start)
      )
  }) %>%
  # designate incomplete cv series
  mutate(cv_full_series = nuts2 %in% dat$country_selection) %>%
  # remove incomplete cv series
  filter(cv_full_series | cv_id == 0) %>%
  arrange(cv_id, nuts2, date) %>%
  ungroup()

# Select and rename -----------------------------------------------

dat$ready_for_export <-
  dat$monthly_births_cv %>%
  select(
    # id variables
    cv_id = cv_id,
    obs_id = obs_id,
    # strata
    region_lvl_1 = nuts1,
    region_lvl_2 = nuts2,
    year = year,
    month = month,
    month_fac = month_fac,
    # observations
    observed = observed_births,
    exposure = personmonths,
    # additional calendar variables
    date = date,
    origin_date = origin_date,
    origin_weeks = origin_weeks,
    origin_date_test = origin_date_test,
    weeks_since_test_start = weeks_since_test_start,
    # flags
    cv_sample = cv_sample,
    cv_full_series = cv_full_series
  ) %>%
  arrange(cv_id, obs_id)

# Validation plots ------------------------------------------------

# plot training-test split
fig$training_test_split <-
  dat$ready_for_export %>%
  mutate(
    isna = is.na(observed) | is.na(exposure)
  ) %>%
  filter(region_lvl_2 == 'AT11') %>%
  ggplot(aes(x = date, y = cv_id)) +
  geom_path(
    aes(color = cv_sample, size = isna,
        group = interaction(cv_id, cv_sample)),
    alpha = 1
  ) +
  facet_wrap(~region_lvl_2, ncol = 5) +
  scale_x_date(date_breaks = '2 year', date_labels = '%y', expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:5, labels = c('0 (Total)', 1:5)) +
  labs(x = 'Year', y = 'CV ID') +
  guides(color = 'none', size = 'none') +
  scale_color_manual(values = c(figspec$colors$sample)) +
  scale_size_manual(values = c(`TRUE` = 0.5, `FALSE` = 2)) +
  labs(
    title = 'Data coverage and training-test split',
    subtitle = 'Test grey, training red. Thin lines indicate NAs in death, exposure, holiday or temperature variables.'
  ) +
  figspec$MyGGplotTheme(panel_border = TRUE, grid = 'x', minor_grid = 'x')
fig$training_test_split

# Export ----------------------------------------------------------

saveRDS(dat$ready_for_export, file = paths$output$data_cv)

ExportFigure(
  fig$training_test_split, paths$output$tmpdir, 'training_test_split',
  device = 'pdf'
)
