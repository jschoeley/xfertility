# Global constants and functions

# Figure specification --------------------------------------------

# fonts
library(showtext)
font_add_google('Roboto', 'roboto')
font_add_google('Roboto Condensed', 'robotocondensed')
showtext_auto()

figspec <- list()
figspec <- within(figspec, {
  
  # color coding
  colors = list(
    sample =
      c(
        training = "grey30",
        test = "red"
      ),
    sex =
      c(
        `Male` = "#004B87",
        `Female` = "#c60c30"
      )
  )
  
  # figure dimensions in mm
  fig_dims = list(width = 180)
  
  # ggplot theme
  # ggplot theme by Jonas Schöley
  MyGGplotTheme <-
    function (
      size = 8,
      family = 'roboto',
      scaler = 1,
      axis = 'x',
      panel_border = FALSE,
      grid = 'y',
      minor_grid = '',
      show_legend = TRUE,
      ar = NA,
      axis_title_just = 'rt',
      axis_ticks = TRUE
    ) {
      
      size_med = size*scaler
      size_sml = round(size*0.7)*scaler
      base_linesize = 0.3*scaler
      
      # justification of axis titles
      xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, 
                   l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
      yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, 
                   l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
      
      list(
        theme_minimal(base_size = size_med, base_family = family),
        theme(
          # basic
          text = element_text(color = 'black'),
          line = element_line(size = base_linesize, lineend = 'square'),
          # axis
          axis.title = element_text(size = size_med, face = 'bold'),
          axis.title.x = element_text(hjust = xj),
          axis.title.y = element_text(hjust = yj),
          axis.title.y.right = element_text(hjust = yj, angle = 90),
          axis.text = element_text(size = size_med, color = 'black'),
          # strips
          strip.text = element_text(color = 'black', size = size_med),
          strip.background = element_blank(),
          # plot
          title = element_text(face = 'bold'),
          plot.subtitle = element_text(color = 'black', size = size_med, face = 'bold'),
          plot.caption = element_text(color = 'black', size = size_sml, face = 'plain'),
          plot.background = element_blank(),
          panel.background = element_blank(),
          #plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
          # grid
          panel.grid = element_blank()
        ),
        if (isTRUE(axis_ticks)) {
          theme(axis.ticks = element_line(size = rel(0.5), color = 'black'))
        },
        if (identical(grid, 'y')) {
          theme(panel.grid.major.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(grid, 'x')) {
          theme(panel.grid.major.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(grid, 'xy') | identical(grid, 'yx')) {
          theme(panel.grid.major.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'),
                panel.grid.major.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'y')) {
          theme(panel.grid.minor.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'x')) {
          theme(panel.grid.minor.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'xy') | identical(grid, 'yx')) {
          theme(panel.grid.minor.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'),
                panel.grid.minor.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (isTRUE(panel_border)) {
          theme(
            panel.border =
              element_rect(fill = NA)
          )
        },
        if (!isTRUE(show_legend)) {
          theme(legend.position = 'none')
        },
        if (axis == 'x') {
          theme(
            axis.line.x = element_line(linetype = 1, color = 'black')
          )
        },
        if (axis == 'y') {
          theme(
            axis.line.y = element_line(linetype = 1, color = 'black')
          )
        },
        if (axis == 'xy') {
          theme(
            axis.line = element_line(linetype = 1, color = 'black')
          )
        },
        if (!is.na(ar)) {
          theme(
            aspect.ratio = ar
          )
        }
      )
    }
  
})


# Global functions figures ----------------------------------------

#' Export ggplot
#' 
#' @author Jonas Schöley
ExportFigure <-
  function(figure,
           path,
           filename,
           width = 170,
           height = 100,
           scale = 1,
           device = 'png',
           dpi = 300,
           add_date = FALSE) {
    require(ggplot2)
    
    if (missing(filename)) {
      filename <- tolower(gsub('\\.', '_', make.names(deparse(substitute(figure)))))
    }
    if (isTRUE(add_date)) {
      filename <- paste0(Sys.Date(), '-', filename)
    }
    
    arguments <-
      list(
        filename = paste0(filename, '.', device),
        plot = figure,
        path = path,
        width = width,
        height = height,
        units = "mm",
        scale = scale,
        dpi = dpi,
        device = device
      )
    if (device == 'pdf') {
      arguments$useDingbats <- FALSE 
    }
    
    do.call(ggsave, arguments)
  }

#' Export ggplots Stored in List
#' 
#' @author Jonas Schöley
ExportFiguresFromList <- function(lst, path, ...) {
  figure_names <- tolower(gsub('\\.+', '_', make.names(names(lst))))
  Fun <- function (figure, filename, ...) {
    ExportFigure(figure = figure, filename = filename, ...)
  }
  purrr::pwalk(
    list(lst, figure_names),
    Fun, path = path, ...
  )
}

# Global functions date -------------------------------------------

#' Create Unique Row ID
#'
#' @param region_nuts2 NUTS-2 region code.
#' @param year Positive Integer.
#' @param month Positive Integer.
#'
#' @return
#' String with fixed length row ID constructed from input.
#'
#' @examples
#' GenerateRowID('PT15', 2020, 10)
GenerateRowID <- function(region_nuts2, year, month) {
  region_id <- sapply(region_nuts2, function (x) {
    expanded_region <- '----'
    substr(expanded_region, 1, nchar(x)) <- x
    return(expanded_region)
  })
  year_id <- sprintf('%04d', year)
  month_id <- sprintf('%02d', month)
  
  row_id <- paste0(region_nuts2, year_id, month_id)
  
  return(row_id)
}

#' Calculate Weeks Since Some Origin Date
#'
#' @param date Date string.
#' @param origin_date Date string.
#' @param week_format Either 'integer' for completed weeks or
#' 'fractional' for completed fractional weeks.
#'
#' @return Time difference in weeks.
#'
#' @author Jonas Schöley
#'
#' @examples
#' # My age in completed weeks
#' WeeksSinceOrigin(Sys.Date(), '1987-07-03')
WeeksSinceOrigin <-
  function(date, origin_date, week_format = "integer") {
    require(ISOweek)
    fractional_weeks_since_origin <-
      as.double(difftime(
        as.Date(date),
        as.Date(origin_date),
        units = "weeks"
      ))
    switch(
      week_format,
      fractional = fractional_weeks_since_origin,
      integer = as.integer(fractional_weeks_since_origin)
    )
  }

#' Convert Month of Year to Date
#'
#' @param year Year integer.
#' @param month Week of year integer (1 to 53).
#' @param monthday Day of month integer.
#'
#' @return A date object.
#' 
#' @author Jonas Schöley
#'
#' @examples
#' # the first Week of 2020 actually starts Monday, December 30th 2019
#' MonthdateToDate(2020, 1, 1)
MonthdateToDate <- function (year, month, monthday = 1) {
  require(lubridate)
  make_date(year, month, monthday)
}

#' Calculate Excess Count Residuals and Residual Summaries
#'
#' @param df A data frame.
#' @param date Name of date variable.
#' @param observed Name of observed count variable.
#' @param predicted Name of predicted count variable.
#' @param cv_id Name of variable identifying cross-validation series.
#' @param ... Names of stratum variables. Counts in strata not
#'            mentioned here will be summed across.
#'
#' @return Data frames with residuals over time and strata and derived
#'         error summaries.
CountResiduals <-
  function (df, date, observed, predicted, cv_id, ...) {
    
    .date <- enquo(date)
    .observed <- enquo(observed)
    .predicted <- enquo(predicted)
    .cv_id <- enquo(cv_id)
    .strata <- enquos(...)
    
    # calculate raw residuals of (cumulative) counts
    # after aggregation of counts into specified strata
    residual_raw <-
      df %>%
      # aggregate to desired population
      group_by(!!.cv_id, !!!.strata, !!.date) %>%
      summarise(
        observed = sum(!!.observed),
        predicted = sum(!!.predicted)
      ) %>%
      # calculate residuals in (cumulative) counts
      # by cv_id and stratum
      group_by(!!.cv_id, !!!.strata) %>%
      arrange(!!.date) %>%
      mutate(
        resid_e = observed - predicted,
        resid_pe = resid_e / observed * 100,
        observed_cum = cumsum(observed),
        predicted_cum = cumsum(predicted),
        resid_e_cum = observed_cum - predicted_cum,
        resid_pe_cum = resid_e_cum / observed_cum * 100
      )
    
    # summarise weekly and cumulative residuals into measures of
    # error, bias, and variance
    residual_summary <-
      residual_raw %>%
      # summarise residuals across cv_id
      group_by(!!!.strata, !!.date) %>%
      summarise(
        ### CUMULATIVE COUNTS
        # raw error
        me_cum = mean(resid_e_cum),
        se_cum = sd(resid_e_cum),
        # percentage error
        mpe_cum = mean(resid_pe_cum),
        spe_cum = sd(resid_pe_cum),
        # absolute error
        mae_cum = mean(abs(resid_e_cum)),
        sae_cum = sd(abs(resid_e_cum)),
        # absolute percentage error
        mape_cum = mean(abs(resid_pe_cum)),
        sape_cum = sd(abs(resid_pe_cum)),
        
        ### WEEKLY COUNTS
        # raw error
        me_count = mean(resid_e),
        se_count = sd(resid_e),
        # percentage error
        mpe_count = mean(resid_pe),
        spe_count = sd(resid_pe),
        # absolute error
        mae_count = mean(abs(resid_e)),
        sae_count = sd(abs(resid_e)),
        # absolute percentage error
        mape_count = mean(abs(resid_pe)),
        sape_count = sd(abs(resid_pe))
      ) %>%
      ungroup()
    
    return(list(residual_raw = residual_raw, residual_summary = residual_summary))
    
  }
