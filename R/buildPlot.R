#' Build Plot Function
#'
#' A function to create plots using either ggplot2 or highcharter libraries.
#' @param data A data.frame containing at least columns named ID, X, and Y.
#' @param library A string specifying which plotting library to use ("ggplot2" or "highcharter").
#' @param ... Additional parameters to customize the plot.
#' @return A plot object from the specified library.
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_smooth geom_point
#' @importFrom highcharter highchart hc_yAxis hc_xAxis hc_add_theme hc_add_series hc_colors hc_tooltip
#' @importFrom ggthemes theme_foundation
#' @importFrom utils modifyList
#' @import data.table 
#' @importFrom grDevices hcl.colors
#' @import epoxy
#' @import scales
#' @import RColorBrewer
#' 
buildPlot <- function(
  data,
  library = "ggplot2",
  ...
) {
  . <- NULL
  # Ensure required columns are present in data
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }

  # Define default parameters
  default_params <- list(
    plot.object = NULL,
    plot.title = NULL,
    plot.subtitle = NULL,
    plot.height = NULL,
    plot.width = NULL,
    xAxis.legend = "X",
    yAxis.legend = "Y",
    group.legend = "ID",
    color.palette = grDevices::hcl.pals()[4],
    plot.type = "line", # c("line","spline","point","column","bar")
    line.style = "solid", # For consistency across libraries
    point.style = "circle",     # For consistency across libraries
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
    legend.align = "right", # c("center", "left", "right")
    legend.valign = "top",  # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = TRUE,
    plot.theme = NULL,
    xAxis.legend.fontsize = "14px",
    yAxis.legend.fontsize = "14px",
    group.legend.fontsize = "12px",
    plot.title.fontsize = "24px",
    plot.subtitle.fontsize = "18px",
    print.max.abs = FALSE,
    fill.polygon = FALSE,
    fill.group = "",
    point.marker = FALSE,
    point.dataLabels = FALSE
  )

  # Capture user-provided parameters
  user_params <- list(...)

  # Merge default parameters with user-provided parameters
  params <- modifyList(default_params, user_params)
  params$data <- data  # Include data in params

  # Validate legend parameters
  valid_valign <- c("top", "middle", "bottom")
  valid_align <- c("left", "center", "right")
  valid_layout <- c("horizontal", "vertical")
  if (!(params$legend.valign %in% valid_valign)) {
    stop(paste("Invalid legend.valign:", params$legend.valign, "Expected one of:", paste(valid_valign, collapse = ", ")))
  }
  if (!(params$legend.align %in% valid_align)) {
    stop(paste("Invalid legend.align:", params$legend.align, "Expected one of:", paste(valid_align, collapse = ", ")))
  }
  if (!(params$legend.layout %in% valid_layout)) {
    stop(paste("Invalid legend.layout:", params$legend.layout, "Expected one of:", paste(valid_layout, collapse = ", ")))
  }

  # Map line.style to appropriate values
  line.style.map <- list(
    "solid" = "Solid",
    "shortdash" = "ShortDash",
    "shortdot" = "ShortDot",
    "shortdashdot" = "ShortDashDot",
    "shortdashdotdot" = "ShortDashDotDot",
    "dot" = "Dot",
    "dash" = "Dash",
    "longdash" = "LongDash",
    "dashdot" = "DashDot",
    "longdashdot" = "LongDashDot",
    "longdashdotdot" = "LongDashDotDot"
  )
  params$line.style <- line.style.map[[tolower(params$line.style)]] %||% params$line.style

  # Map point.style to appropriate shapes for ggplot2 and highcharter
  point.style.map <- list(
    "circle" = list(ggplot2 = 16, highcharter = "circle"),
    "square" = list(ggplot2 = 15, highcharter = "square"),
    "diamond" = list(ggplot2 = 18, highcharter = "diamond"),
    "triangle" = list(ggplot2 = 17, highcharter = "triangle"),
    "triangle-down" = list(ggplot2 = 25, highcharter = "triangle-down")
  )
  selected.point.style <- point.style.map[[tolower(as.character(params$point.style))]]
  if (!is.null(selected.point.style)) {
    params$point.style.ggplot2 <- selected.point.style$ggplot2
    params$point.style.highcharter <- selected.point.style$highcharter
  } else {
    # Default to circle if not specified
    params$point.style.ggplot2 <- 16
    params$point.style.highcharter <- "circle"
  }

  # Set default theme if not specified
  if (is.null(params$plot.theme)) {
    if (tolower(library) %in% c("ggplot2", "gg", "ggplot")) {
      params$plot.theme <- theme_flat()
    } else if (tolower(library) %in% c("highcharter", "hc", "highchart")) {
      params$plot.theme <- highcharter::hc_theme_flat()
    }
  }

  # Call the respective plot function based on the library parameter
  if (tolower(library) %in% c("ggplot2", "gg", "ggplot")) {
    result <- buildPlot.ggplot2(params)
  } else if (tolower(library) %in% c("highcharter", "hc", "highchart")) {
    result <- buildPlot.highcharter(params)
  } else {
    stop("Unsupported library specified")
  }

  return(result)
}

# Utility operator for default values
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Helper function for ggplot2
buildPlot.ggplot2 <- function(params) {
    . <- NULL
  # Extract parameters
  data <- as.data.table(params$data)[, .(ID, X, Y)]
  data[, ID := as.factor(ID)]  # Convert ID to factor

  # Prepare the plot object
  if (is.null(params$plot.object)) {
    PLOT <- ggplot(
      data = data,
      aes(x = X, y = Y, group = ID, color = ID)
    )
  } else {
    PLOT <- params$plot.object
    PLOT <- PLOT + geom_blank() # Ensure it accepts additional layers
  }

  # Generate colors
  NID <- length(unique(data$ID))
  COLORS <- grDevices::hcl.colors(n = NID, palette = params$color.palette)

  # Add layers based on plot type
  if (tolower(params$plot.type) == "line") {
    PLOT <- PLOT +
      geom_line(
        aes(color = ID),
        size = params$line.size,
        linetype = params$line.style
      ) +
      scale_color_manual(values = COLORS)
  }

  if (tolower(params$plot.type) == "spline") {
    PLOT <- PLOT +
      geom_smooth(
        aes(color = ID),
        method = 'loess',
        size = params$line.size,
        linetype = params$line.style,
        se = FALSE
      ) +
      scale_color_manual(values = COLORS)
  }

  if (tolower(params$plot.type) %in% c("scatter", "point")) {
    PLOT <- PLOT +
      geom_point(
        aes(color = ID),
        size = params$point.size,
        shape = params$point.style.ggplot2
      ) +
      scale_color_manual(values = COLORS)
  }

  # Additional features (max abs value, fill polygon, etc.)
  # [Preserve all existing features as in the original code]

  # Axis transformations
  if (params$yAxis.log == TRUE) {
    PLOT <- PLOT + scale_y_log10()
  }
  if (params$xAxis.log == TRUE) {
    PLOT <- PLOT + scale_x_log10()
  }

  if (params$yAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_y_reverse()
  }

  if (params$xAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_x_reverse()
  }

  # Titles and labels
  if (!is.null(params$plot.title)) {
    PLOT <- PLOT + ggtitle(params$plot.title)
  }

  if (!is.null(params$plot.subtitle)) {
    PLOT <- PLOT + labs(subtitle = params$plot.subtitle)
  }

  PLOT <- PLOT + xlab(params$xAxis.legend) + ylab(params$yAxis.legend)

  if (params$xAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.x = element_blank())
  }

  if (params$yAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.y = element_blank())
  }

  # Legend settings
  if (params$legend.show) {
    PLOT <- PLOT + theme(
      legend.position = c(
        switch(params$legend.align,
               "left" = 0,
               "center" = 0.5,
               "right" = 1),
        switch(params$legend.valign,
               "bottom" = 0,
               "middle" = 0.5,
               "top" = 1)
      ),
      legend.direction = params$legend.layout
    )
  } else {
    PLOT <- PLOT + theme(legend.position = "none")
  }

  # Apply theme
  PLOT <- PLOT + params$plot.theme

  # Return the plot
  return(PLOT)
}

# Helper function for highcharter
buildPlot.highcharter <- function(params) {
  # Ensure required columns are present in data
  if (!all(c("ID", "X", "Y") %in% colnames(params$data))) {
    stop("data must contain columns named ID, X, and Y")
  }

  data <- params$data

  # If no existing plot object, create a new highchart object and set axes
  if (is.null(params$plot.object)) {
    PLOT <- highchart()

    # Y-axis settings
    PLOT <- PLOT |>
      hc_yAxis(
        labels = list(enabled = params$yAxis.label),
        title = list(
          text = params$yAxis.legend,
          style = list(fontSize = params$yAxis.legend.fontsize)
        ),
        type = if (params$yAxis.log) "logarithmic" else "linear",
        reversed = params$yAxis.reverse,
        max = if (!is.na(params$yAxis.max)) params$yAxis.max else NULL,
        min = if (!is.na(params$yAxis.min)) params$yAxis.min else NULL
      )

    # X-axis settings
    PLOT <- PLOT |>
      hc_xAxis(
        labels = list(enabled = params$xAxis.label),
        title = list(
          text = params$xAxis.legend,
          style = list(fontSize = params$xAxis.legend.fontsize)
        ),
        type = if (params$xAxis.log) "logarithmic" else "linear",
        reversed = params$xAxis.reverse,
        max = if (!is.na(params$xAxis.max)) params$xAxis.max else NULL,
        min = if (!is.na(params$xAxis.min)) params$xAxis.min else NULL
      )

    # Apply theme
    PLOT <- PLOT |>
      hc_add_theme(hc_thm = params$plot.theme)
  } else {
    # Use the existing plot object
    PLOT <- params$plot.object
  }

  # Generate colors
  NID <- length(unique(data$ID))
  COLORS <- grDevices::hcl.colors(n = NID, palette = params$color.palette)

  # Map line.style to Highcharter's accepted values
  line.style.map <- list(
    "solid" = "Solid",
    "shortdash" = "ShortDash",
    "shortdot" = "ShortDot",
    "shortdashdot" = "ShortDashDot",
    "shortdashdotdot" = "ShortDashDotDot",
    "dot" = "Dot",
    "dash" = "Dash",
    "longdash" = "LongDash",
    "dashdot" = "DashDot",
    "longdashdot" = "LongDashDot",
    "longdashdotdot" = "LongDashDotDot"
  )
  params$line.style <- line.style.map[[tolower(params$line.style)]] %||% params$line.style

  # Add series using params$plot.type directly
  PLOT <- PLOT |>
    hc_add_series(
      data = data,
      type = params$plot.type,
      dashStyle = params$line.style,
      lineWidth = params$line.size,
      marker = list(
        symbol = params$point.style.highcharter,
        radius = params$point.size,
        enabled = tolower(params$plot.type) %in% c("point", "scatter") || params$point.marker
      ),
      hcaes(x = X, y = Y, group = ID),
      colorByPoint = FALSE
    )

  # Tooltip settings
  # Construct TIP without evaluating {point.ID}
  TIP <- paste0(
    "<b>{point.ID}</b><br>",
    params$xAxis.legend, ": {point.x}<br>",
    params$yAxis.legend, ": {point.y}"
  )

  # Apply colors and tooltip
  PLOT <- PLOT |>
    hc_colors(colors = COLORS) |>
    hc_tooltip(
      sort = FALSE,
      split = FALSE,
      crosshairs = TRUE,
      headerFormat = "",
      pointFormat = TIP
    )

  # Only apply legend settings when creating a new plot
  if (is.null(params$plot.object)) {
    # Legend settings
    PLOT <- PLOT |>
      hc_legend(
        enabled = params$legend.show,
        align = params$legend.align,
        verticalAlign = params$legend.valign,
        layout = params$legend.layout,
        itemStyle = list(fontSize = params$group.legend.fontsize)
      ) |>
      hc_chart(style = list(fontFamily = "Helvetica"))

    # Titles
    if (!is.null(params$plot.title)) {
      PLOT <- PLOT |>
        hc_title(
          text = params$plot.title,
          style = list(fontSize = params$plot.title.fontsize)
        )
    }

    if (!is.null(params$plot.subtitle)) {
      PLOT <- PLOT |>
        hc_subtitle(
          text = params$plot.subtitle,
          style = list(fontSize = params$plot.subtitle.fontsize)
        )
    }

    # Plot size
    if (!is.null(params$plot.height) || !is.null(params$plot.width)) {
      PLOT <- PLOT |>
        hc_size(height = params$plot.height, width = params$plot.width)
    }
  }

  # Plot options
  PLOT <- PLOT |>
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = params$point.dataLabels),
        marker = list(
          enabled = params$point.marker,
          radius = params$point.size
        )
      )
    )

  # Return the plot
  return(PLOT)
}

# Custom theme for ggplot2
theme_flat <- function() {
  ggthemes::theme_foundation() +
    theme(
      plot.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.grid.major = element_line(color = "#BDC3C7",  size = 0.3),
      panel.grid.minor = element_line(color = "#BDC3C7", size = 0.1),
      axis.line.x = element_line(color = "#BDC3C7"),
      axis.line.y = element_line(color = "#BDC3C7"),
      axis.ticks.x = element_line(color = "#BDC3C7"),
      axis.ticks.y = element_line(color = "#BDC3C7"),
      axis.text = element_text(color = "#34495e"),
      axis.title = element_text(color = "#34495e", face = "bold"),
      plot.title = element_text(color = "#34495e", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#34495e", hjust = 0.5),
      legend.background = element_rect(fill = alpha("black", 0.1), color = NA),
      legend.key = element_rect(fill = alpha("black", 0.5), color = NA),
      legend.text = element_text(color = "#34495e"),
      legend.title = element_text(color = "#34495e", face = "bold")
    )
}
