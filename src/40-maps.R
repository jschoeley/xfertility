# Map regional excess births

# Init ------------------------------------------------------------

library(tidyverse)
library(yaml)
library(lubridate)
library(eurostat)
library(sf)
library(rnaturalearth)
# map simplification tools, requires dependencies external to R
# sudo apt install protobuf-compiler libprotobuf-dev libjq-dev libv8-dev
library(rmapshaper)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  config = 'src/config.yaml',
  excess = 'tmp/excess.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  out = 'out',
  background_map = 'out/background_map.rds',
  euro_geo_nuts2 = 'out/euro_geo_nuts2.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  map_limits = c(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)
  crs = 3035
  n_sim = 100
  width = unit(8, 'cm')
  height = width*0.8375
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Data for background map -----------------------------------------

# download geospatial data for European, Asian and African countries
# for use as background map
dat$background_map <-
  ne_countries(
    continent = c('europe', 'asia', 'africa'),
    returnclass = 'sf', scale = 50
  ) %>%
  # re-project
  st_transform(crs = cnst$crs) %>%
  # pseudo-buffer regions to avoid self-intersection errors
  st_buffer(0) %>%
  # crop to Europe
  st_crop(
    xmin = cnst$map_limits[['xmin']], xmax = cnst$map_limits[['xmax']],
    ymin = cnst$map_limits[['ymin']], ymax = cnst$map_limits[['ymax']]
  )

saveRDS(dat$background_map, paths$output$background_map)

# European NUTS-2 geodata -----------------------------------------

# download geodata on nuts-2 regions
dat$euro_geo_nuts2 <-
  get_eurostat_geospatial(
    output_class = 'sf', resolution = '60', nuts_level = 2, year = 2016
  ) %>%
  st_transform(crs = cnst$crs) %>%
  st_buffer(0) %>%
  st_crop(
    xmin = cnst$map_limits[['xmin']], xmax = cnst$map_limits[['xmax']],
    ymin = cnst$map_limits[['ymin']], ymax = cnst$map_limits[['ymax']]
  ) %>%
  # simplify to save space
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>%
  select(id, name = NUTS_NAME, geometry)

saveRDS(dat$euro_geo_nuts2, paths$output$euro_geo_nuts2)

# Load regional counts --------------------------------------------

dat$excess <- read_rds(paths$input$excess)

# Join counts with geodata ----------------------------------------

dat$sf_nuts2_excess_significance <-
  dat$euro_geo_nuts2 %>%
  left_join(dat$excess, by = c('id' = 'NUTS2code'))

# Value-by-alpha --------------------------------------------------

fig$pscore_map <- list()

fig$pscore_map$cnst <- list(
  model_id = 'mav',
  timebase = 't',
  yearmonth = c('2020-11', '2020-12', '2021-1', '2021-2', '2021-2',
                '2021-3', '2021-4')
)

fig$pscore_map$data <-
  dat$sf_nuts2_excess_significance %>%
  mutate(yearmonth = paste0(Year,'-',Month)) %>%
  filter(yearmonth %in% fig$pscore_map$cnst$yearmonth) %>%
  rename(
    q05 = paste0('psc_', fig$pscore_map$cnst$timebase, '_q05_', fig$pscore_map$cnst$model_id),
    q25 = paste0('psc_', fig$pscore_map$cnst$timebase, '_q25_', fig$pscore_map$cnst$model_id),
    q50 = paste0('psc_', fig$pscore_map$cnst$timebase, '_q50_', fig$pscore_map$cnst$model_id),
    q75 = paste0('psc_', fig$pscore_map$cnst$timebase, '_q75_', fig$pscore_map$cnst$model_id),
    q95 = paste0('psc_', fig$pscore_map$cnst$timebase, '_q95_', fig$pscore_map$cnst$model_id)
  )

# use lighter and more desaturated colors for life-expectancy changes
# with uncertain direction
fig$pscore_map$scale_breaks <- c(-Inf, -0.1, 0, 0.1, Inf)

# create divergent color scale with 4 effect levels
# and 2 certainty levels
fig$pscore_map$certain_colors <- RColorBrewer::brewer.pal(4, 'BrBG')
fig$pscore_map$uncertain_colors <-
  prismatic::clr_alpha(fig$pscore_map$certain_colors, alpha = 0.2)
fig$pscore_map$uncertainty_palette_4x2 <- tibble(
  id = c(sapply(1:2, paste0, 1:4)),
  rgb = c(fig$pscore_map$uncertain_colors, fig$pscore_map$certain_colors)
)

# generate legend
fig$pscore_map$valuebyalpha_legend <- expand_grid(
  certainty = 1:2,
  effect = 1:4
) %>%
  mutate(rgb = fig$pscore_map$uncertainty_palette_4x2$rgb) %>%
  ggplot() +
  geom_tile(aes(y = certainty, x = effect, fill = rgb)) +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(1, 2), labels = c('<90%', '>90%')) +
  scale_x_continuous(
    breaks = c(1.5, 2.5, 3.5), labels = c('-10%', '0', '+10%'),
    position = 't'
  ) +
  coord_fixed(0.5) +
  labs(
    y = 'Certainty', x = 'Effect size',
    title = 'Percent excess births'
  ) +
  theme_minimal(base_family = 'robotocondensed') +
  theme(
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.margin = unit(c(0,0,0,0),'mm')
  )

# Rescale [0,1] range vector to range [xmin, xmax]
ReScaleInv <- function (x, xmin, xmax) {
  x*(xmax-xmin) + xmin
}

# generate map
fig$pscore_map$plot <-
  fig$pscore_map$data %>%
  # map the colors to the regions based on effect size and certainty
  mutate(
    # dim the colors if we are less than 90% certain about
    # the direction of the result
    dim = ifelse((sign(q05) + sign(q95)) == 0, 1, 2),
    # discretized effect size
    effect = cut(q50, breaks = fig$pscore_map$scale_breaks, labels = 1:4),
    # rgb color
    color = factor(
      paste0(dim, effect),
      levels = fig$pscore_map$uncertainty_palette_4x2$id,
      labels = fig$pscore_map$uncertainty_palette_4x2$rgb
    ) %>% as.character()
  ) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = color), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  # add legend to map
  annotation_custom(
    ggplotGrob(fig$pscore_map$valuebyalpha_legend),
    xmin =
      ReScaleInv(0.6, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    xmax =
      ReScaleInv(0.95, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    ymin =
      ReScaleInv(0.54, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']]),
    ymax =
      ReScaleInv(0.99, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']])
  ) +
  facet_wrap(~yearmonth) +
  scale_fill_identity() +
  coord_sf(expand = FALSE, datum = NA) +
  labs(
    title = 'Percent excess births in Europe Nov 2020 through April 2021',
    caption = '@jschoeley'
  ) +
  theme(
    strip.background = element_blank(),
    text = element_text(family = 'roboto'),
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$pscore_map$plot

ggsave(
  'out/pscore_map.png', fig$pscore_map$plot,
  height = 10, width = 18,
  dpi = 300
)
