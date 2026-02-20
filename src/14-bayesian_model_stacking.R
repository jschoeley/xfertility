# Perform Bayesian model stacking of posterior predictive densities
#
# See DOI: 10.1214/17-BA1091 for Bayesian model averaging via stacking
# of predictive densities.
# A. Calculate the likelihood for each model from the model predictions
# over the holdout (test) data,
# B. from this we get the weight for each model.

library(yaml)
library(readr)
library(dplyr)
library(tidyr)
library(qs)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  glob = 'src/00-global_objects.R',
  fitted_models = 'tmp/13-expected_cv_sim.rds',
  config = 'src/config.yaml',
  model_metadata = 'src/model_metadata.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  expected_with_bms_sim = 'out/14-expected_with_bms_sim.qs',
  model_weights = 'out/14-model_weights.csv',
  model_loglike = 'out/14-model_loglike.csv'
)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  config = read_yaml(paths$input$config)
  model_metadata = read_csv(paths$input$model_metadata)
  models_to_include = model_metadata[model_metadata$include==1,][['code']]
  config = read_yaml(paths$input$config)
  ndraws = config$ndraws
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Load data -------------------------------------------------------

# load data for cross validation
dat$fitted_models <- readRDS(paths$input$fitted_models)

# Derived stacking weights ----------------------------------------

bms <- list()

# fitted cross-validation series, only test data
bms$holdoutfit <-
  dat$fitted_models |>
  filter(
    cv_id != 0,
    #model_id %in% cnst$models_to_include,
    error_while_fit == FALSE
  ) |>
  unnest(predictions) |>
  ungroup() |>
  filter(cv_sample == 'test') |>
  select(model_id, observed, starts_with('simulated'))

# A. calculate the likelihood for each model from the model predictions
# over the holdout (test) data.
# B. from this we get the weight for each model

# A. log likelihood for each data point
{
  bms$N <- nrow(bms$holdoutfit)
  #bms$holdoutfit <- bms$holdoutfit[sample(N),]
  bms$holdoutfit$lppd <- NA
  bms$simnames <- grep('^simulated.+', colnames(bms$holdoutfit))
  for (i in 1:bms$N) {
    # estimate density of predictive distribution from samples
    D <- hist(unlist(bms$holdoutfit[i,bms$simnames]),
              plot = FALSE, breaks = 10)
    y <- bms$holdoutfit[i,][['observed']]
    # get log density/likelihood of observed data point
    bms$holdoutfit[i, 'lppd'] <- log(D[['density']][
      which.min(abs(D[['mids']]-y))])
    cat(paste(bms$holdoutfit[i,][['lppd']], floor(i/bms$N*100)),'\n')
  }
  bms$modelloglikelihood <- bms$holdoutfit[,c('model_id', 'lppd')]
  bms$modelloglikelihoodsum <-
    bms$modelloglikelihood |>
    group_by(model_id) |>
    summarise(ll = sum(lppd[!is.infinite(lppd)], na.rm = TRUE))
}

bms$modelloglikelihood <- readRDS('pvt/modelloglikelihood.rds')

# B. find model weights w such that the combined likelihood of the
# data from a linear combination of models gets maximized
{
  X <-
    bms$modelloglikelihood |>
    pivot_wider(
      names_from = 'model_id',
      values_from = 'lppd',
      values_fn = list
    ) |>
    unnest(cols = everything()) |>
    as.matrix()
  
  infinite_lppd <- apply(X, 1, function (x) any(is.infinite(x)))
  X <- X[!infinite_lppd,]
  
  K <- ncol(X)
  w <- rep(1/K, K)+0.1
  objective <- function (w) {
    sum(X%*%w)  
  }
  
  # constraints (unit sum of positive values)
  C1 <- matrix(1, nrow = 1, ncol = K)
  C2 <- diag(K)
  Cc1 <- 1
  Cc2 <- rep(0, K)
  C <- rbind(C1, C2)
  Cc <- c(Cc1, Cc2)
  
  Co <- constrOptim(
    w, objective, NULL, ui = C, ci = Cc,
    control = list(fnscale = -1, maxit = 1e5)
  )
  
  w <- Co$par/sum(Co$par)
  model_weights <- tibble(
    model_id = colnames(X),
    weight = w
  )
}

# Perform model stacking ------------------------------------------

# for each observation sample 5000 posterior predictions
# from the predictions of the various models weighted by the
# stacking weights
dat$expected_with_bms_sim <-
  dat$fitted_models |>
  left_join(model_weights, by = c('model_id'))

dat$fitted_models <- NULL # free memory

dat$expected_with_bms_sim <-
  dat$expected_with_bms_sim |>
  group_by(cv_id) |>
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
        as.matrix(.x[.x$model_id == k,][['predictions']][[1]][
          ,c('predicted', paste0('simulated', 1:n_sim))])
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
    predictions_modelaveraged[,1] <-
      rowMeans(predictions_modelaveraged, na.rm = TRUE)
    
    bind_rows(
      .x,
      tibble(
        model_id = 'BMS', model_class = 'bms',
        model_para = list(NA),
        predictions = list(
          cbind(
            .x[1,][['predictions']][[1]] %>%
              select(-all_of(prediction_names)),
            predictions_modelaveraged
          ) %>% as_tibble()
        ),
        error_while_fit = FALSE
      )          
    )
    
  }) %>%
  ungroup()

gc()

# Export ----------------------------------------------------------

write_csv(model_weights, paths$output$model_weights)
write_csv(bms$modelloglikelihoodsum, paths$output$model_loglike)
qsave(
  dat$expected_with_bms_sim,
  file = paths$output$expected_with_bms_sim
)
