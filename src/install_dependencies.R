pkgs <- c(
  'qs',
  'ggplot2',
  'ISOweek',
  'lubridate',
  'purrr',
  'showtext',
  'tidyverse',
  'yaml',
  'MASS',
  'mgcv',
  'dplyr',
  'doParallel',
  'foreach',
  'glue',
  'gridExtra',
  'readr',
  'tidyr',
  'prismatic',
  'scales',
  'patchwork',
  'eurostat',
  'RColorBrewer',
  'rmapshaper',
  'rnaturalearth',
  'sf',
  'coda',
  'mcmc',
  'MCMCpack'
)

install.packages(
  pkgs,
  dep = TRUE
)
