# Parameterize expected death models
#
# A list specifying all the models to be tested, and their
# parametrizations. See specify_models.R for the exact
# implementation of the models.

# Init ------------------------------------------------------------

library(dplyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$output <- list(
  mod_para = 'tmp/mod_para.rds'
)

# Specifications of models to test --------------------------------

# Just a big list specifying all the models to be tested,
# and their parametrizations. See specify_models.R for the exact
# implementation of the models.
mod_para <-
  tribble(
    
    ~model_id, ~model_class, ~model_para,

    'GLR5yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),

    'GLR3yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 3
    ),
    
    'GLR7yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 7
    ),
    
    'GLC5yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),
    
    'GLC3yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 3
    ),
    
    'GLC7yl', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 7
    ),

    'GLR5yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),
    
    'GLR3yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 3
    ),
    
    'GLR7yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1) +
          offset(log(exposure))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 7
    ),
    
    'GLC5yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),
    
    'GLC3yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 3
    ),
    
    'GLC7yq', 'gam', list(
      formula = formula(
        observed ~
          # time trend
          origin_weeks + origin_weeks^2 +
          # single coefficient for every 2 months
          as.factor(((month-1)/2)%/%1)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 7
    )
        
  )

# Export ----------------------------------------------------------

saveRDS(mod_para, file = paths$output$mod_para)
