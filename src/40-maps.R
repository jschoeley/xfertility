# Map regional excess births

# Init ------------------------------------------------------------

library(tidyverse)
library(yaml)
library(readr)
library(lubridate)
library(eurostat)
library(sf)
library(rnaturalearth)
library(giscoR)
# map simplification tools, requires dependencies external to R
# sudo apt install protobuf-compiler libprotobuf-dev libjq-dev libv8-dev

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = 'tmp',
  config = 'src/config.yaml',
  excess = 'out/21-excess_by_period_region.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  out = 'out',
  background_map = 'out/40-background_map.rds',
  euro_geo_nuts2 = 'out/40-euro_geo_nuts2_merged.rds',
  pscoremap_a_pdf = 'out/40-pscoremap_a.pdf',
  pscoremap_a_rds = 'out/40-pscoremap_a.rds',
  pscoremap_a_csv = 'out/40-pscoremap_a.csv',
  pscoremap_b_pdf = 'out/40-pscoremap_b.pdf',
  pscoremap_b_rds = 'out/40-pscoremap_b.rds',
  pscoremap_b_csv = 'out/40-pscoremap_b.csv'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  map_limits = c(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)
  crs = 3035
  width = unit(8, 'cm')
  height = width*0.8375
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# Rescale [0,1] range vector to range [xmin, xmax]
ReScaleInv <- function (x, xmin, xmax) {
  x*(xmax-xmin) + xmin
}

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

# download geodata on nuts-2 regions (2016 & 2021 definitions)
dat$euro_geo_nuts2_2016 <-
  get_eurostat_geospatial(
    output_class = 'sf', resolution = '20',
    nuts_level = 2, year = 2016, crs = '3035'
  )
dat$euro_geo_nuts2_2021 <-
  get_eurostat_geospatial(
    output_class = 'sf', resolution = '20',
    nuts_level = 2, year = 2021, crs = '3035'
  )

# merge definitions as needed
dat$euro_geo_nuts2_merged <-
  bind_rows(
    filter(dat$euro_geo_nuts2_2016, !NUTS_ID %in% 'HR04'),
    filter(dat$euro_geo_nuts2_2021, NUTS_ID %in% c('HR02','HR05','HR06'))
  )

# crop
dat$euro_geo_nuts2_merged <-
  dat$euro_geo_nuts2_merged %>%
  st_transform(crs = cnst$crs) %>%
  st_buffer(0) %>%
  st_crop(
    xmin = cnst$map_limits[['xmin']], xmax = cnst$map_limits[['xmax']],
    ymin = cnst$map_limits[['ymin']], ymax = cnst$map_limits[['ymax']]
  ) %>%
  # simplify to save space
  #ms_simplify(keep = 0.05, keep_shapes = TRUE) %>%
  select(id, name = NUTS_NAME, geometry)

# adjust map
dat$euro_geo_nuts2_merged <-
  bind_rows(
    dat$euro_geo_nuts2_merged,
    st_sf(
      id = 'UKI1',
      name = 'Inner London',
      geometry = st_union(filter(dat$euro_geo_nuts2_merged, id %in% c('UKI3', 'UKI4')))
    ),
    st_sf(
      id = 'UKI2',
      name = 'Outer London',
      geometry = st_union(filter(dat$euro_geo_nuts2_merged, id %in% c('UKI5', 'UKI6', 'UKI7')))
    ),
    st_sf(
      id = 'PL9',
      name = 'Makroregion Województwo Mazowieckie',
      geometry = st_union(filter(dat$euro_geo_nuts2_merged, id %in% c('PL91', 'PL92')))
    ),
    st_sf(
      id = 'ITH1/ITH2',
      name = ' Trentino-Alto Adige/Südtirol',
      geometry = st_union(filter(dat$euro_geo_nuts2_merged, id %in% c('ITH1', 'ITH2')))
    )
  )

# Load regional counts --------------------------------------------

dat$excess <- read_rds(paths$input$excess)

# Join counts with geodata ----------------------------------------

dat$sf_nuts2_excess_significance <-
  dat$euro_geo_nuts2_merged %>%
  left_join(dat$excess, by = c('id' = 'region_lvl_2'))

# Value-by-alpha --------------------------------------------------

# create divergent color scale with 4 effect levels
# and 2 certainty levels
certain_colors <- RColorBrewer::brewer.pal(4, 'BrBG')
uncertain_colors <-
  prismatic::clr_alpha(
    rep(certain_colors[c(1,4)], each = 2), alpha = 0.15
  )
uncertainty_palette_4x2 <- tibble(
  id = c(sapply(1:2, paste0, 1:4)),
  rgb = c(uncertain_colors, certain_colors)
)

# create legend
valuebyalpha_legend <- expand_grid(
  certainty = 1:2,
  effect = 1:4
) %>%
  mutate(rgb = uncertainty_palette_4x2$rgb) %>%
  ggplot() +
  geom_tile(aes(y = certainty, x = effect, fill = rgb)) +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(1, 2), labels = c('<95%', '≥95%')) +
  scale_x_continuous(
    breaks = c(1.5, 2.5, 3.5), labels = c('-10%', '0', '+10%'),
    position = 'top'
  ) +
  coord_fixed(0.5) +
  labs(
    y = 'Certainty in\ndirection of effect', x = '% change births',
  ) +
  #theme_minimal(base_family = 'robotocondensed', base_size = 6) +
  theme(
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.margin = unit(c(0,0,0,0),'mm')
  )

fig$pscore_map <- list(
  A = list(
    name = 'A',
    period = c(`I: November 2020 through January 2021` = 'I',
               `II: February 2021 through March 2021` = 'II',
               `III: April 2021 through June 2021` = 'III',
               `I+II+III: November 2020 through June 2021` = 'I+II+III')
  ),
  B = list(
    name = 'B',
    period = c(`IV: July 2021 through October 2021` = 'IV',
               `V: November 2021 through January 2022` = 'V',
               `IV+V: July 2021 through January 2022` = 'IV+V',
               `Total: November 2020 through January 2022` = 'Total')
  )
)

lapply(fig$pscore_map, function (panel) {
  
  const <- list(
    model_id = 'bms',
    timebase = 't'
  )
  
  # discretize effect size into four bins
  scale_breaks <- c(-Inf, -0.1, 0, 0.1, Inf)
  
  fig$pscore_map[[panel$name]]$data <<-
    dat$sf_nuts2_excess_significance %>%
    filter(period %in% panel$period) %>%
    mutate(period = factor(period, panel$period, names(panel$period))) |>
    rename(
      q05 = paste0('psc_', const$timebase, '_q05_', const$model_id),
      q25 = paste0('psc_', const$timebase, '_q25_', const$model_id),
      q50 = paste0('psc_', const$timebase, '_q50_', const$model_id),
      q75 = paste0('psc_', const$timebase, '_q75_', const$model_id),
      q95 = paste0('psc_', const$timebase, '_q95_', const$model_id),
      pd  = paste0('psc_', const$timebase, '_pd_', const$model_id)
    )
  
  # generate map
  fig$pscore_map[[panel$name]]$plot <<-
    fig$pscore_map[[panel$name]]$data %>%
    # map the colors to the regions based on effect size and certainty
    mutate(
      # dim the colors if we are less than 95% certain about
      # the direction of the result
      # 1 (uncertain); 2 (certain)
      dim = ifelse(pd < 0.95, 1, 2),
      # discretized effect size
      effect = cut(q50, breaks = scale_breaks, labels = 1:4),
      # rgb color
      color = factor(
        paste0(dim, effect),
        levels = uncertainty_palette_4x2$id,
        labels = uncertainty_palette_4x2$rgb
      ) %>% as.character()
    ) %>%
    ggplot() +
    geom_sf(data = dat$background_map, fill = 'grey92', color = NA) +
    geom_sf(aes(fill = color), color = NA) +
    geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
    # add legend to map
    annotation_custom(
      ggplotGrob(valuebyalpha_legend),
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
    facet_wrap(~period) +
    scale_fill_identity() +
    coord_sf(expand = FALSE, datum = NA) +
    theme(
      strip.background = element_blank(),
      panel.background = element_rect(colour = 'black', fill = 'white'),
      plot.background = element_blank(),
      plot.margin = unit(rep(0.1,4), 'cm')
    )
  #fig$pscore_map[[panel$name]]$plot
  
})

# Export ----------------------------------------------------------

saveRDS(dat$euro_geo_nuts2_merged, paths$output$euro_geo_nuts2)

ggsave(
  paths$output$pscoremap_a_pdf, fig$pscore_map$A$plot,
  device = 'pdf',
  height = 10, width = 10, bg = 'white',
  dpi = 300
)

saveRDS(fig$pscore_map$A, paths$output$pscoremap_a_rds)

write_csv(
  fig$pscore_map$A$data |> as_tibble() |> select(
    psc_t_q05_bms = q05,
    psc_t_q25_bms = q25,
    psc_t_q50_bms = q50,
    psc_t_q75_bms = q75,
    psc_t_q95_bms = q95
  ),
  paths$output$pscoremap_a_csv
)

ggsave(
  paths$output$pscoremap_b_pdf, fig$pscore_map$B$plot,
  device = 'pdf',
  height = 10, width = 10, bg = 'white',
  dpi = 300
)

saveRDS(fig$pscore_map$B, paths$output$pscoremap_b_rds)

write_csv(
  fig$pscore_map$B$data |> as_tibble() |> select(
    psc_t_q05_bms = q05,
    psc_t_q25_bms = q25,
    psc_t_q50_bms = q50,
    psc_t_q75_bms = q75,
    psc_t_q95_bms = q95
  ),
  paths$output$pscoremap_b_csv
)
