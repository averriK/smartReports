# nolint start
#' @title Build a highchart plot
#' @description Build a highchart plot using separate data sources for lines and points.
#'
#' Supports the plotting of line and scatter series. Includes optional arearange shading between
#' two line groups if indicated in data.lines$fill and interpolation is handled via the approx function 
#' (which can be customized using the interpolation.method argument). Fallback behaviors:
#' if an invalid line.type is provided, "line" is used; if an invalid or missing line style is found, 
#' "solid" is used; if an invalid color.palette is provided, a default Highcharts palette is chosen.
#'
#' @param data.lines A data.table containing columns "ID", "X", "Y", optional "style", optional "fill". 
#'                   If NULL, no lines will be plotted. 
#' @param data.points A data.table containing columns "ID", "X", "Y", optional "style". 
#'                    If NULL, no scatter points will be plotted.
#' @param line.type A string indicating whether lines should be "line" or "spline". 
#'                  If an invalid value is provided, it reverts to "line".
#' @param line.type DEPRECATED
#' @param library DEPRECATED
#' @param plot.type DEPRECATED
#' @param plot.title A string for the plot title
#' @param plot.subtitle A string for the plot subtitle
#' @param plot.height A numeric for the plot height
#' @param plot.width A numeric for the plot width
#' @param xAxis.legend A string for the x-axis legend
#' @param yAxis.legend A string for the y-axis legend
#' @param group.legend A string for the group legend
#' @param color.palette A string for the color palette (must exist in grDevices::hcl.pals())
#' @param line.style DEPRECATED; line styles are taken from data.lines$style. 
#'                   However, a default style is used if data.lines$style is missing or invalid.
#' @param point.style DEPRECATED; point styles are taken from data.points$style. 
#'                    However, a default style is used if data.points$style is missing or invalid.
#' @param line.size A numeric for the line width
#' @param point.size A numeric for the point size
#' @param xAxis.log A logical for the x-axis log scale
#' @param yAxis.log A logical for the y-axis log scale
#' @param xAxis.reverse A logical for the x-axis reverse
#' @param yAxis.reverse A logical for the y-axis reverse
#' @param xAxis.max A numeric for the x-axis max
#' @param yAxis.max A numeric for the y-axis max
#' @param xAxis.min A numeric for the x-axis min
#' @param yAxis.min A numeric for the y-axis min
#' @param xAxis.label A logical for the x-axis label
#' @param yAxis.label A logical for the y-axis label
#' @param legend.layout A string for the legend layout
#' @param legend.align A string for the legend align
#' @param legend.valign A string for the legend valign
#' @param legend.show A logical for the legend show
#' @param plot.save A logical for the plot save
#' @param plot.theme A highchart theme object
#' @param xAxis.legend.fontsize A string for the x-axis legend fontsize
#' @param yAxis.legend.fontsize A string for the y-axis legend fontsize
#' @param group.legend.fontsize A string for the group legend fontsize
#' @param plot.title.fontsize A string for the plot title fontsize
#' @param plot.subtitle.fontsize A string for the plot subtitle fontsize
#' @param print.max.abs A logical for printing max absolute Y labels as annotations (only for lines)
#' @param point.marker DEPRECATED; point markers are now controlled via data.points$style. 
#' @param point.dataLabels A logical for whether data labels appear for points
#' @param plot.filename A string for the plot filename, if saving
#' @param interpolation.method A string specifying the interpolation method used by approx(). 
#'                             Defaults to "linear". Other valid values include "constant", "spline", etc. 
#'                             For large datasets, you can also consider downsampling or chunking externally.
#'
#' @return A highchart object if either data.lines or data.points is provided. 
#'         Returns NULL if both are NULL, with a soft warning.
#' @importFrom grDevices hcl.pals hcl.colors    
#' @importFrom stats setNames approx
#' @import highcharter 
#' @importFrom htmlwidgets saveWidget
#' @export buildPlot
#' 
buildPlot <- function(
  library = NULL, #DEPRECATED.
  plot.type = NULL, # DEPRECATED. Replaced by line.type.
  line.style = "Solid",      # DEPRECATED, only used if data.lines$style is missing
  point.style = "circle",    # DEPRECATED, only used if data.points$style is missing
  point.marker = TRUE,      # DEPRECATED, replaced by data.points$style
  data.lines = NULL,
  data.points = NULL,
  line.type = "line",  # Must be "line" or "spline"
  plot.title = NULL,
  plot.subtitle = NULL,
  plot.height = NULL,
  plot.width = NULL,
  xAxis.legend = "X",
  yAxis.legend = "Y",
  group.legend = "ID",
  color.palette = "Dark 3",
  line.size = 1,
  point.size = 3,
  xAxis.log = FALSE,
  yAxis.log = FALSE,
  xAxis.reverse = FALSE,
  yAxis.reverse = FALSE,
  xAxis.max = NA,
  yAxis.max = NA,
  xAxis.min = NA,
  yAxis.min = NA,
  xAxis.label = TRUE,
  yAxis.label = TRUE,
  legend.layout = "horizontal",
  legend.align = "right", # c("center","left","right")
  legend.valign = "top",  # c("top","middle","bottom")
  legend.show = TRUE,
  plot.save = FALSE,
  plot.theme = NULL,
  xAxis.legend.fontsize = "14px",
  yAxis.legend.fontsize = "14px",
  group.legend.fontsize = "12px",
  plot.title.fontsize = "24px",
  plot.subtitle.fontsize = "18px",
  print.max.abs = FALSE,
  
  point.dataLabels = FALSE,
  plot.filename = NULL,
  interpolation.method = "linear"
) {
  # Soft check if both data.lines and data.points are NULL
  if (is.null(data.lines) && is.null(data.points)) {
    warning("No data provided for lines or points. Returning NULL.")
    return(NULL)
  }

  # Validate numeric inputs (warn & set default, instead of stop)
  if (!is.numeric(line.size) || line.size <= 0) {
    warning(
      sprintf("line.size should be a positive numeric. Using default (1) instead of '%s'.", line.size)
    )
    line.size <- 1
  }
  if (!is.numeric(point.size) || point.size <= 0) {
    warning(
      sprintf("point.size should be a positive numeric. Using default (3) instead of '%s'.", point.size)
    )
    point.size <- 3
  }

  # Validate color palette
  if (!color.palette %in% grDevices::hcl.pals()) {
    warning("Invalid color palette. Using default.")
    color.palette <- grDevices::hcl.pals()[4]
  }

  # Prepare name mappings for line dash styles
  # Lowercased keys map to Highcharts dashStyle
  LINE.STYLE <- list(
    "solid"          = "Solid",
    "dashed"         = "Dash",
    "dotted"         = "Dot",
    "dotdash"        = "DashDot",
    "longdash"       = "LongDash",
    "twodash"        = "Dash",
    "shortdash"      = "ShortDash",
    "shortdot"       = "ShortDot",
    "shortdashdot"   = "ShortDashDot",
    "longdashdotdot" = "LongDashDotDot"
  )

  # Prepare name mappings for point marker styles
  POINT.STYLE <- list(
    "circle"        = "circle",
    "square"        = "square",
    "diamond"       = "diamond",
    "triangle"      = "triangle",
    "triangle-down" = "triangle-down"
  )

  # Validate line.type
  line.type <- tolower(line.type)
  if (!(line.type %in% c("line", "spline"))) {
    warning(sprintf("Provided line.type='%s' is invalid. Using 'line' by default.", line.type))
    line.type <- "line"
  }

  # Validate data inputs
  validateData <- function(data, data_name) {
    if (!is.null(data)) {
      if (!all(c("ID", "X", "Y") %in% colnames(data))) {
        stop(sprintf("%s must contain columns named ID, X, and Y if not NULL.", data_name))
      }
      if (!is.numeric(data$Y)) {
        stop(sprintf("Error in buildPlot.highchart(): %s$Y must be numeric.", data_name))
      }
    }
  }

  validateData(data.lines, "data.lines")
  validateData(data.points, "data.points")

  # Simplify color mapping (collect all IDs for consistent color usage)
  ALL.IDS <- unique(c(
    if (!is.null(data.lines)) data.lines$ID,
    if (!is.null(data.points)) data.points$ID
  ))
  ID.COLOR.MAP <- stats::setNames(
    grDevices::hcl.colors(
      n = length(ALL.IDS), 
      palette = color.palette
    ),
    ALL.IDS
  )

  # Initialize plot object
  plot.object <- highchart() |>
    hc_xAxis(
      labels = list(enabled = xAxis.label),
      title = list(
        text = xAxis.legend,
        style = list(fontSize = xAxis.legend.fontsize)
      ),
      type = if (xAxis.log) "logarithmic" else "linear",
      reversed = xAxis.reverse,
      max = if (!is.na(xAxis.max)) xAxis.max else NULL,
      min = if (!is.na(xAxis.min)) xAxis.min else NULL
    ) |>
    hc_yAxis(
      labels = list(enabled = yAxis.label),
      title = list(
        text = yAxis.legend,
        style = list(fontSize = yAxis.legend.fontsize)
      ),
      type = if (yAxis.log) "logarithmic" else "linear",
      reversed = yAxis.reverse,
      max = if (!is.na(yAxis.max)) yAxis.max else NULL,
      min = if (!is.na(yAxis.min)) yAxis.min else NULL
    )

  # Theme handling
  if (!is.null(plot.theme)) {
    plot.object <- plot.object |> hc_add_theme(plot.theme)
  } else {
    plot.object <- plot.object |> hc_add_theme(hc_theme_flat())
  }

  # Titles
  if (!is.null(plot.title)) {
    plot.object <- plot.object |>
      hc_title(
        text = plot.title,
        style = list(fontSize = plot.title.fontsize)
      )
  }
  if (!is.null(plot.subtitle)) {
    plot.object <- plot.object |>
      hc_subtitle(
        text = plot.subtitle,
        style = list(fontSize = plot.subtitle.fontsize)
      )
  }

  # Plot size
  if (!is.null(plot.height) || !is.null(plot.width)) {
    plot.object <- plot.object |>
      hc_size(height = plot.height, width = plot.width)
  }

  # Legend
  plot.object <- plot.object |>
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) |>
    hc_chart(style = list(fontFamily = "Helvetica"))

  # ============ LINES =============
  if (!is.null(data.lines)) {
    # For each unique group in data.lines, add a series
    for (gid in unique(data.lines$ID)) {
      #sub_data <- data.lines[ID == gid]

      # Determine dash style from sub_data$style, if present
      dash_candidate <- if ("style" %in% names( data.lines[ID == gid])) {
        tolower(as.character( data.lines[ID == gid]$style[1]))
      } else {
        tolower(line.style)  # fallback
      }

      dash_style <- LINE.STYLE[[dash_candidate]]
      if (is.null(dash_style)) {
        # If invalid, warn & use default
        warning(sprintf("Invalid line style '%s' for ID='%s'. Using '%s'.", 
                        dash_candidate, gid, line.style))
        dash_style <- line.style
      }

      # Build line series
      plot.object <- plot.object |>
        hc_add_series(
          data =  data.lines[ID == gid],
          type = line.type,
          hcaes(x = X, y = Y),
          name = as.character(gid),
          color = ID.COLOR.MAP[as.character(gid)],
          dashStyle = dash_style,
          lineWidth = line.size,
          marker = list(enabled = FALSE)
        )
    }

    # Modification to handle area between curves
    ID.FILL <- unique(data.lines[fill == TRUE, ID])

    # If exactly two IDs are marked for filling
    if (length(ID.FILL) == 2) {
      COMMON.X <- sort(unique(c(data.lines[ID == ID.FILL[1]]$X, data.lines[ID == ID.FILL[2]]$X)))
      IC1 <- approx(
        data.lines[ID == ID.FILL[1]]$X,
        data.lines[ID == ID.FILL[1]]$Y,
        xout = COMMON.X,
        method = interpolation.method
      )
      IC2 <- approx(
        data.lines[ID == ID.FILL[2]]$X,
        data.lines[ID == ID.FILL[2]]$Y,
        xout = COMMON.X,
        method = interpolation.method
      )

      plot.object <- plot.object |>
        hc_add_series(
          data = data.frame(
            X = COMMON.X,
            LOW = pmin(IC1$y, IC2$y),
            HIGH = pmax(IC1$y, IC2$y)
          ),
          type = "arearange",
          hcaes(x = X, low = LOW, high = HIGH),
          name = paste("Area between", ID.FILL[1], "and", ID.FILL[2]),
          color = ID.COLOR.MAP[as.character(ID.FILL[1])],
          fillOpacity = 0.3  # Adjust transparency as needed
        )
    } else if (length(FILL.IDS) > 2) {
      warning("More than two IDs marked for filling. Only first two will be used.")
    }
  }

  # ============ POINTS =============
  if (!is.null(data.points)) {
    # For each unique group in data.points, add a scatter series
    for (gid in unique(data.points$ID)) {
      #sub_data <- data.points[ID == gid]

      # Determine point style from sub_data$style (fallback to point.style if missing)
      sym_candidate <- if ("style" %in% names(data.points[ID == gid])) {
        tolower(as.character(data.points[ID == gid]$style[1]))
      } else {
        tolower(point.style)  # fallback
      }

      symbol_style <- POINT.STYLE[[sym_candidate]]
      if (is.null(symbol_style)) {
        warning(sprintf("Invalid point style '%s' for ID='%s'. Using '%s'.", 
                        sym_candidate, gid, point.style))
        symbol_style <- point.style
      }

      # Build scatter series
      plot.object <- plot.object |>
        hc_add_series(
          data = data.points[ID == gid],
          type = "scatter", # always scatter for points
          hcaes(x = X, y = Y),
          name = as.character(gid),
          color = ID.COLOR.MAP[as.character(gid)],
          marker = list(
            symbol = symbol_style,
            radius = point.size
          )
        )
    }
  }

  # Simplify tooltip
  plot.object <- plot.object |>
    hc_tooltip(
      sort = FALSE,
      split = FALSE,
      crosshairs = TRUE,
      headerFormat = "",
      pointFormat = sprintf(
        "<b>{point.series.name}</b><br>%s: {point.x}<br>%s: {point.y}",
        xAxis.legend,
        yAxis.legend
      )
    ) |>
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = point.dataLabels)
      )
    )

  # Print max absolute value using annotations (applied to data.lines only)
  if (print.max.abs && !is.null(data.lines)) {
    # Data check
    if (!is.numeric(data.lines$Y)) {
      stop("Y in data.lines must be numeric for max-abs calculations.")
    }
    data_abs <- data.lines[, .SD[which.max(abs(Y))], by = ID]
    plot.object <- plot.object |>
      hc_annotations(
        list(
          labels = lapply(seq_len(nrow(data_abs)), function(i) {
            list(
              point = list(
                xAxis = 0, yAxis = 0,
                x = data_abs$X[i], y = data_abs$Y[i]
              ),
              text = paste0("Max Abs: ", round(data_abs$Y[i], 2))
            )
          })
        )
      )
  }

  # Save if requested
  if (plot.save) {
    if (is.null(plot.filename)) 
      plot.filename <- "plot.html"

    # Because this is package code, you might be using:
    # htmlwidgets::saveWidget(widget = plot, file = plot.filename)
    # Or handle it elsewhere in your package.
    saveWidget(widget = plot.object, file = plot.filename)
  }

  return(plot.object)
}
# nolint end
