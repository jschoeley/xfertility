# Fit expected birth models
#
# Fit all models specified in 12-parameterize_expected_count_models.R
# across all cross-validation sets and predict from models.

# Init ------------------------------------------------------------

library(tidyverse)
library(yaml)
library(glue)
library(foreach)
library(doParallel)

# Constants -------------------------------------------------------

set.seed(1987)

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  config = 'src/config.yaml',
  mod_def = 'tmp/11-mod_def.rds',
  mod_para = 'tmp/12-mod_para.rds',
  glob = 'src/00-global_objects.R',
  data_cv = 'tmp/10-data_cv.rds',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  expected_cv = 'tmp/13-expected_cv_sim.rds',
  log = 'tmp/13-fitted_models_log.txt'
)

# global functions and constants
source(paths$input$glob)

# model specifications
ModDef <- readRDS(paths$input$mod_def)
# model parametrizations
mod_para <- readRDS(paths$input$mod_para)

# constants
cnst <- list()
cnst <- within(cnst,{
  config = read_yaml(paths$input$config)
  model_metadata = read_csv(paths$input$model_metadata)
  # how many threads used to fit models?
  cpu_nodes = 14
  # how many draws from the posterior predictive distribution?
  ndraws = config$ndraws
})

dat <- list()
fig <- list()

# setup parallel computation
cnst$cl <- makeCluster(cnst$cpu_nodes, outfile = paths$output$log)
registerDoParallel(cnst$cl)

# Load data -------------------------------------------------------

# load data for cross validation
data_cv <- readRDS(paths$input$data_cv)

# Prepare data for fit --------------------------------------------

# merge data with model definitions
dat$fit_data <-
  data_cv %>%
  nest(data = c(-cv_id)) %>%
  expand_grid(mod_para)

# Fit models and predict ------------------------------------------

# fit models
dat$fitted_models <-
  # iterate in parallel models and CV id
  foreach(
    x = iter(dat$fit_data, by = 'row'),
    .combine = bind_rows,
    .packages = c('dplyr', 'tidyr', 'mgcv')
  ) %dopar% {suppressPackageStartupMessages({
    
    cat(format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
        ' Fit CV set', x$cv_id,
        ' for ', x$model_id, '\n', sep = '')
    
    # prepare training and prediction data
    # single region, cv id and model
    input_dat <- x[,'data'][[1]][[1]]
    # model parametrization
    model_para <- x$model_para[[1]]

    # fit models and capture errors
    result <- tryCatch({
      
      # fit GAM model
      
      if (x$model_class == 'gam') {
        
        predictions <- ModDef$CountGAM(
          df = input_dat,
          formula = model_para$formula,
          family = model_para$family,
          col_sample = 'cv_sample',
          # fit separately by country
          col_stratum = 'region_lvl_2',
          n_years_for_training = model_para$n_years_for_training,
          col_year = 'year',
          nsim = cnst$ndraws, simulate_beta = TRUE, simulate_y = TRUE,
          method = 'REML'
        )
        
      }
      
      # # fit LMM model
      # 
      # if (x$model_class == 'lmm') {
      # 
      #   predictions <- ModDef$CountGAM(
      #     df = input_dat,
      #     formula = model_para$formula,
      #     family = model_para$family,
      #     col_sample = 'cv_sample',
      #     col_stratum = 'region_lvl_1',
      #     n_years_for_training = model_para$n_years_for_training,
      #     col_year = 'year',
      #     nsim = cnst$ndraws, simulate_beta = TRUE, simulate_y = TRUE,
      #     method = 'REML'
      #   )
      # 
      # }
      
      # return result if fitting succeeded
      
      result_if_no_error <- bind_cols(
        x,
        tibble(
          predictions = list(predictions),
          error_while_fit = FALSE,
          error_message = NA
        )
      )
      return(result_if_no_error)
      
    },
    
    # return result if fitting did not succeed
    
    error = function(e) {
      cat(format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
          ' Error on CV set ', x$cv_id,
          ' for ', x$model_id, ': ', geterrmessage(), '\n')
      # return same object as fitted model, but with NA predictions
      input_dat[,c('predicted',paste0('simulated', 1:cnst$ndraws))] <- NA
      result_if_error <- bind_cols(x, tibble(
        predictions = list(input_dat),
        error_while_fit = TRUE,
        error_message = geterrmessage()
      ))
      return(result_if_error)
      
    }) # end of tryCatch()
    
    return(result)
    
  })}

stopCluster(cnst$cl)

dat$fitted_models <-
  dat$fitted_models |>
  select(-data)

# Plot observed vs. fitted ----------------------------------------

dat$fitted_models %>%
  filter(!error_while_fit) %>%
  unnest(predictions) %>%
  filter(region_lvl_2 == 'AT11') %>%
  group_by(region_lvl_2, model_id) %>%
  group_walk(~{
    
    expected <-
      .x %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        observed = sum(observed),
        predicted = sum(predicted)
      ) %>%
      ungroup()
    simulated <-
      .x %>%
      pivot_longer(cols = starts_with('simulated'),
                   names_to = 'sim_id', values_to = 'simulated') %>%
      group_by(cv_id, date, cv_sample, sim_id) %>%
      summarise(
        simulated = sum(simulated)
      ) %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        q05 = quantile(simulated, 0.05, na.rm = TRUE),
        q95 = quantile(simulated, 0.95, na.rm = TRUE)
      ) %>%
      ungroup()
    
    fig_dat <- left_join(expected, simulated)

    fig[[paste(.y[[1]], .y[[2]])]] <<-
      fig_dat %>%
      ggplot(aes(x = date)) +
      geom_ribbon(aes(x = date, ymin = q05, ymax = q95),
                  fill = 'grey70', color = NA,
                  data = . %>% filter(cv_sample == 'test')) +
      geom_point(aes(color = cv_sample, y = observed),
                 size = 0.3) +
      geom_line(aes(x = date, y = predicted, alpha = cv_sample),
                color = 'red',
                data = . %>% filter(cv_sample == 'test')) +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      scale_alpha_manual(values = c(training = 0.3, test = 1)) +
      scale_color_manual(values = figspec$colors$sample) +
      facet_grid(cv_id~'') +
      guides(color = 'none', alpha = 'none') +
      figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE) +
      labs(
        x = NULL, y = 'Weekly Births',
        title = paste(.y[[1]], .y[[2]])
      )
  })

# Exports ---------------------------------------------------------

saveRDS(dat$fitted_models, file = paths$output$expected_cv)

ggsave(
  filename = '13-fitted_vs_observed_cv.pdf',
  path = paths$output$tmpdir,
  plot = gridExtra::marrangeGrob(fig, nrow=1, ncol=1), 
  width = 15, height = 9
)
