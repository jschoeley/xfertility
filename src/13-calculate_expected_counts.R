# Fit expected deaths models

# Init ------------------------------------------------------------

library(tidyverse)
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
  mod_def = 'tmp/mod_def.rds',
  mod_para = 'tmp/mod_para.rds',
  glob = 'src/00-global_objects.R',
  data_cv = 'tmp/data_cv.rds',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  expected_cv = 'tmp/expected_cv.rds',
  log = 'tmp/fitted_models_log.txt'
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
  model_metadata = read_csv(paths$input$model_metadata)
  # how many threads used to fit models?
  cpu_nodes = 14
  # how many draws from the posterior predictive distribution?
  ndraws = 500
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

# Define country specific parameters ------------------------------

# here we specify country-specific exceptions from the default models.

{

  # # for Island average death count
  # # estimate the average over 12 consecutive weeks
  # idx <- with(dat$fit_data, model_id == 'AVC' & region_iso == 'IS')
  # patched_model <- dat$fit_data[idx,][['model_para']][[1]]
  # patched_model[['models']][[1]] <- formula(
  #   deaths_observed ~ as.factor(((iso_week-1)/12)%/%1)
  # )
  # dat$fit_data[idx,][['model_para']] <- list(patched_model)
  # 
  # # for Island average death rate
  # # estimate the average over 12 consecutive weeks
  # idx <- with(dat$fit_data, model_id == 'AVR' & region_iso == 'IS')
  # patched_model <- dat$fit_data[idx,][['model_para']][[1]]
  # patched_model[['models']][[1]] <- formula(
  #   deaths_observed ~ as.factor(((iso_week-1)/12)%/%1) +
  #     offset(log(personweeks))
  # )
  # dat$fit_data[idx,][['model_para']] <- list(patched_model)
  
}

# Fit models and predict ------------------------------------------

# fit models
dat$fitted_models <-
  # iterate in parallel models and CV id
  foreach(
    x = iter(dat$fit_data, by = 'row'),
    .combine = bind_rows,
    .packages = c('dplyr', 'tidyr', 'INLA', 'mgcv')
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
      
      # fit LMM model
      
      if (x$model_class == 'lmm') {
        
        predictions <- ModDef$CountGAM(
          df = input_dat,
          formula = model_para$formula,
          family = model_para$family,
          col_sample = 'cv_sample',
          col_stratum = 'region_lvl_1',
          n_years_for_training = model_para$n_years_for_training,
          col_year = 'year',
          nsim = cnst$ndraws, simulate_beta = TRUE, simulate_y = TRUE,
          method = 'REML'
        )
        
      }
      
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

# Perform model averaging -----------------------------------------

# for each observation sample 500 posterior predictions
# uniformly from the predictions of the various models
dat$fitted_models_with_mav <-
  dat$fitted_models %>%
  left_join(
    cnst$model_metadata %>% select(code, weight),
    by = c('model_id' = 'code')
  ) %>%
  select(-data) %>%
  group_by(cv_id) %>%
  group_modify(~{

    cv_id = .y[['cv_id']]
    obs_id = .x[1,][['predictions']][[1]][['obs_id']]
    n_rows = nrow(.x[1,][['predictions']][[1]])
    n_sim = cnst$ndraws
    unique_models = unique(.x$model_id)
    n_models = length(unique_models)
    predictions_by_model <-
      array(
        dim = c(n_rows, n_sim+1, n_models),
        dimnames = list('obs_id' = obs_id,
                        'sim_id' = 0:n_sim,
                        'model_id' = unique_models)
      )
    for (k in unique_models) {
      cat(cv_id, k,'\n')
      predicted_and_simulated <-
        as.matrix(.x[.x$model_id == k,][['predictions']][[1]][,c('predicted', paste0('simulated', 1:n_sim))])
      predictions_by_model[,,k] <- c(predicted_and_simulated)
    }
    # random index matrix selecting for each prediction
    # I <- matrix(
    #   sample(
    #     1:n_models, n_rows*(n_sim+1), replace = TRUE,
    #     prob = .x$weight
    #   ),
    #   n_rows, (n_sim+1)
    # )
    # weights sampled from dirichlet distribution
    I <- matrix(NA, n_rows, (n_sim+1))
    for (j in 2:(n_sim+1)) {
      w <- c(MCMCpack::rdirichlet(1, .x$weight))
      I[,j] <- sample(
        1:n_models, n_rows, replace = TRUE,
        prob = w
      )
    }

    # https://stackoverflow.com/a/39344780
    predictions_modelaveraged <- matrix(
      predictions_by_model[cbind(
        rep(1:n_rows, n_sim+1),
        rep(1:(n_sim+1), each = n_rows),
        c(I)
      )],
      n_rows, n_sim+1
    )
    prediction_names <- c('predicted', paste0('simulated', 1:n_sim))
    colnames(predictions_modelaveraged) <- prediction_names
    predictions_modelaveraged[,1] <- rowMeans(predictions_modelaveraged, na.rm = TRUE)
    
    bind_rows(
      .x,
      tibble(
        model_id = 'MAV', model_class = 'mav',
        model_para = list(NA),
        predictions = list(
          cbind(
            .x[1,][['predictions']][[1]] %>% select(-all_of(prediction_names)),
            predictions_modelaveraged
          ) %>% as_tibble()
        ),
        error_while_fit = FALSE
      )          
    )
    
  })

# Plot observed vs. fitted ----------------------------------------

dat$fitted_models_with_mav %>%
  filter(!error_while_fit) %>%
  unnest(predictions) %>%
  #filter(region_lvl_2 == 'AT11') %>%
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
      geom_ribbon(aes(ymin = q05, ymax = q95),
                  fill = 'grey70', color = NA) +
      geom_point(aes(color = cv_sample, y = observed),
                 size = 0.3) +
      geom_line(aes(y = predicted, alpha = cv_sample),
                color = 'red') +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      scale_alpha_manual(values = c(training = 0.3, test = 1)) +
      scale_color_manual(values = figspec$colors$sample) +
      facet_grid(cv_id~'') +
      guides(color = 'none', alpha = 'none') +
      figspec$MyGGplotTheme(grid = 'xy') +
      labs(
        x = NULL, y = 'Weekly Births',
        title = paste(.y[[1]], .y[[2]])
      )
  })

# Exports ---------------------------------------------------------

saveRDS(dat$fitted_models_with_mav, file = paths$output$expected_cv)

ggsave(
  filename = 'fitted_vs_observed_cv.pdf',
  path = paths$output$tmpdir,
  plot = gridExtra::marrangeGrob(fig, nrow=1, ncol=1), 
  width = 15, height = 9
)
