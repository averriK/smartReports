# nolint start
#' @title Build a highchart plot
#' @description Build a highchart plot using separate data sources for lines and points.
#' @param data.lines A data.table containing columns "ID", "X", "Y", optional "style", optional "fill". 
#'                   If NULL, no lines will be plotted. 
#' @param data.points A data.table containing columns "ID", "X", "Y", optional "style". 
#'                    If NULL, no scatter points will be plotted.
#' @param line.type A string indicating whether lines should be "line" or "spline" (for all line groups).
#' @param plot.title A string for the plot title
#' @param plot.subtitle A string for the plot subtitle
#' @param plot.height A numeric for the plot height
#' @param plot.width A numeric for the plot width
#' @param xAxis.legend A string for the x-axis legend
#' @param yAxis.legend A string for the y-axis legend
#' @param group.legend A string for the group legend
#' @param color.palette A string for the color palette (must exist in grDevices::hcl.pals())
#' @param line.style Deprecated; line styles are taken from data.lines$style. 
#'                   However, a default style is used if data.lines$style is missing or invalid.
#' @param point.style Deprecated; point styles are taken from data.points$style. 
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
#' @param point.marker Deprecated; point markers are now controlled via data.points$style. 
#' @param point.dataLabels A logical for whether data labels appear for points
#' @param plot.filename A string for the plot filename, if saving
#' @param ... Additional arguments (unused)
#' @return A highchart object if either data.lines or data.points is provided. 
#'         Returns NULL if both are NULL, with a soft warning.
#' @export buildPlot
#' 
buildPlot <- function(
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
  line.style = "Solid",      # DEPRECATED, only used if data.lines$style is missing
  point.style = "circle",    # DEPRECATED, only used if data.points$style is missing
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
  point.marker = TRUE,      # DEPRECATED, replaced by data.points$style
  point.dataLabels = FALSE,
  plot.filename = NULL,
  ...
) {
  # Soft check if both data.lines and data.points are NULL
  if (is.null(data.lines) && is.null(data.points)) {
    warning("No data provided for lines or points. Returning NULL.")
    return(NULL)
  }

  # Validate numeric inputs
  if (!is.numeric(line.size) || line.size <= 0) {
    stop("line.size must be a positive number")
  }
  if (!is.numeric(point.size) || point.size <= 0) {
    stop("point.size must be a positive number")
  }

  # Validate color palette
  if (!color.palette %in% grDevices::hcl.pals()) {
    warning("Invalid color palette. Using default.")
    color.palette <- grDevices::hcl.pals()[4]
  }

  # Prepare name mappings for line dash styles
  # Lowercased keys map to Highcharts dashStyle
  LINE_STYLE_MAP <- list(
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
  POINT_STYLE_MAP <- list(
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
  validate_data <- function(data, data_name) {
    if (!is.null(data)) {
      needed_cols <- c("ID", "X", "Y")
      if (!all(needed_cols %in% colnames(data))) {
        stop(sprintf("%s must contain columns named ID, X, and Y if not NULL.", data_name))
      }
      if (!is.numeric(data$Y)) {
        stop(sprintf("Error in buildPlot.highchart(): %s$Y must be numeric.", data_name))
      }
    }
  }

  validate_data(data.lines, "data.lines")
  validate_data(data.points, "data.points")

  # Create color mapping
  COLORS <- grDevices::hcl.colors(
    n = length(unique(c(
      if (!is.null(data.lines)) unique(data.lines$ID) else character(0),
      if (!is.null(data.points)) unique(data.points$ID) else character(0)
    ))), 
    palette = color.palette
  )
  id_color_map <- stats::setNames(
    COLORS, 
    unique(c(
      if (!is.null(data.lines)) unique(data.lines$ID) else character(0),
      if (!is.null(data.points)) unique(data.points$ID) else character(0)
    ))
  )

  # Initialize highchart plot
  PLOT <- highchart() |>
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
    PLOT <- PLOT |> hc_add_theme(plot.theme)
  } else {
    PLOT <- PLOT  |> hc_add_theme(hc_theme_flat())
  }

  # Titles
  if (!is.null(plot.title)) {
    PLOT <- PLOT |>
      hc_title(
        text = plot.title,
        style = list(fontSize = plot.title.fontsize)
      )
  }
  if (!is.null(plot.subtitle)) {
    PLOT <- PLOT |>
      hc_subtitle(
        text = plot.subtitle,
        style = list(fontSize = plot.subtitle.fontsize)
      )
  }

  # Plot size
  if (!is.null(plot.height) || !is.null(plot.width)) {
    PLOT <- PLOT |>
      hc_size(height = plot.height, width = plot.width)
  }

  # Legend
  PLOT <- PLOT |>
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
    unique_line_ids <- unique(data.lines$ID)

    for (gid in unique_line_ids) {
      sub_data <- data.lines[ID == gid]

      # Determine dash style from sub_data$style, if present
      dash_candidate <- if ("style" %in% names(sub_data)) {
        tolower(as.character(sub_data$style[1]))
      } else {
        tolower(line.style)  # fallback
      }

      dash_style <- LINE_STYLE_MAP[[dash_candidate]]
      if (is.null(dash_style)) {
        # If invalid, warn & use default
        warning(sprintf("Invalid line style '%s' for ID='%s'. Using '%s'.", 
                        dash_candidate, gid, line.style))
        dash_style <- line.style
      }

      # Build series
      PLOT <- PLOT |>
        hc_add_series(
          data = sub_data,
          type = line.type,
          hcaes(x = X, y = Y),
          name = as.character(gid),
          color = id_color_map[as.character(gid)],
          dashStyle = dash_style,
          lineWidth = line.size
        )
    }

    # Modification to handle area between curves
    fill_ids <- unique(data.lines[fill == TRUE, ID])
    
    # If exactly two IDs are marked for filling
    if (length(fill_ids) == 2) {
      # Prepare data for the two curves
      data_curve1 <- data.lines[ID == fill_ids[1]]
      data_curve2 <- data.lines[ID == fill_ids[2]]
      
      # Ensure both curves have same X points (interpolate if needed)
      common_x <- sort(unique(c(data_curve1$X, data_curve2$X)))
      
      # Interpolate Y values for both curves to match common X
      interp_curve1 <- approx(data_curve1$X, data_curve1$Y, xout = common_x)
      interp_curve2 <- approx(data_curve2$X, data_curve2$Y, xout = common_x)
      
      # Create a data frame for the area between curves
      area_between_data <- data.frame(
        x = common_x,
        low = pmin(interp_curve1$y, interp_curve2$y),
        high = pmax(interp_curve1$y, interp_curve2$y)
      )
      
      # Add the area series
      PLOT <- PLOT |>
        hc_add_series(
          data = area_between_data,
          type = "arearange",
          hcaes(x = x, low = low, high = high),
          name = paste("Area between", fill_ids[1], "and", fill_ids[2]),
          color = id_color_map[as.character(fill_ids[1])],
          fillOpacity = 0.3  # Adjust transparency as needed
        )
    } else if (length(fill_ids) > 2) {
      warning("More than two IDs marked for filling. Only first two will be used.")
    }
  }

  # ============ POINTS =============
  if (!is.null(data.points)) {
    # For each unique group in data.points, add a scatter series
    unique_point_ids <- unique(data.points$ID)

    for (gid in unique_point_ids) {
      sub_data <- data.points[ID == gid]

      # Determine point style from sub_data$style (fallback to point.style if missing)
      sym_candidate <- if ("style" %in% names(sub_data)) {
        tolower(as.character(sub_data$style[1]))
      } else {
        tolower(point.style)  # fallback
      }

      symbol_style <- POINT_STYLE_MAP[[sym_candidate]]
      if (is.null(symbol_style)) {
        warning(sprintf("Invalid point style '%s' for ID='%s'. Using '%s'.", 
                        sym_candidate, gid, point.style))
        symbol_style <- point.style
      }

      # Build scatter series
      PLOT <- PLOT |>
        hc_add_series(
          data = sub_data,
          type = "scatter", # always scatter for points
          hcaes(x = X, y = Y),
          name = as.character(gid),
          color = id_color_map[as.character(gid)],
          marker = list(
            symbol = symbol_style,
            radius = point.size
          )
        )
    }
  }

  # Tooltip
  TIP <- paste0(
    "<b>{point.series.name}</b><br>",
    xAxis.legend, ": {point.x}<br>",
    yAxis.legend, ": {point.y}"
  )
  PLOT <- PLOT |>
    hc_tooltip(
      sort = FALSE,
      split = FALSE,
      crosshairs = TRUE,
      headerFormat = "",
      pointFormat = TIP
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
    PLOT <- PLOT |>
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
    # htmlwidgets::saveWidget(widget = PLOT, file = plot.filename)
    # Or handle it elsewhere in your package.
    saveWidget(widget = PLOT, file = plot.filename)
  }

  return(PLOT)
}
# nolint end
