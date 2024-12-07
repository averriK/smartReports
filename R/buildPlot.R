# nolint start
#' Build Plot Function
#'
#' A function to create plots using either ggplot2 or highcharter libraries.
#' @param data A data.frame containing at least columns named ID, X, and Y.
#' @param library A string specifying which plotting library to use ("ggplot2" or "highcharter").
#' @param ... Additional parameters to customize the plot.
#' @return A plot object from the specified library.
#' @export
#' @import ggplot2
#' @import highcharter
#' @importFrom ggthemes theme_foundation
#' @importFrom utils modifyList
#' @import data.table
#' @importFrom grDevices hcl.colors
#' @importFrom htmlwidgets saveWidget
#' @import scales
#' @import RColorBrewer

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
    plot.save = FALSE, # Changed to FALSE as default
    plot.theme = NULL,
    xAxis.legend.fontsize = "14px",
    yAxis.legend.fontsize = "14px",
    group.legend.fontsize = "12px",
    plot.title.fontsize = "24px",
    plot.subtitle.fontsize = "18px",
    print.max.abs = FALSE, # Default
    fill.polygon = FALSE,  # Default
    fill.group = "ID",       # Default
    point.marker = TRUE,
    point.dataLabels = FALSE,
    plot.filename = NULL  # For saving plots
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
    "solid" = "solid",
    "dashed" = "dashed",
    "dotted" = "dotted",
    "dotdash" = "dotdash",
    "longdash" = "longdash",
    "twodash" = "twodash"
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
      params$plot.theme <- ggthemes::theme_foundation()
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

  # Fill polygon
  if (params$fill.polygon) {
    PLOT <- PLOT +
      geom_area(
        aes_string(fill = params$fill.group),
        alpha = 0.5
      ) +
      scale_fill_manual(values = COLORS)
  }

  # Print max absolute value
  if (params$print.max.abs) {
    data_abs <- data[, .SD[which.max(abs(Y))], by = ID]
    PLOT <- PLOT +
      geom_text(
        data = data_abs,
        aes(label = paste0("Max Abs: ", round(Y, 2))),
        vjust = -1
      )
  }

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

  # Plot Save
  if (params$plot.save) {
    ggsave(
      filename = params$plot.filename %||% "plot.png",
      plot = PLOT,
      width = params$plot.width %||% 7,
      height = params$plot.height %||% 5
    )
  }

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
  
  # Map plot.type to valid Highcharter types
  plot.type.map <- list(
    "line" = "line",
    "spline" = "spline",
    "area" = "area",
    "areaspline" = "areaspline",
    "column" = "column",
    "bar" = "bar",
    "scatter" = "scatter",
    "point" = "scatter"  # Map "point" to "scatter"
  )
  params$plot.type <- plot.type.map[[tolower(params$plot.type)]] %||% params$plot.type
  
  # Map your line.style to Highcharter's dashStyle directly
  line.style.map <- list(
    "solid" = "Solid",
    "dashed" = "Dash",
    "dotted" = "Dot",
    "dotdash" = "DashDot",
    "longdash" = "LongDash",
    "twodash" = "Dash",
    "shortdash" = "ShortDash",
    "shortdot" = "ShortDot",
    "shortdashdot" = "ShortDashDot",
    "longdashdotdot" = "LongDashDotDot"
  )
  params$line.style <- line.style.map[[tolower(params$line.style)]] %||% params$line.style
  
  # Map your point.style to Highcharter's marker symbols directly
  point.style.map <- list(
    "circle" = "circle",
    "square" = "square",
    "diamond" = "diamond",
    "triangle" = "triangle",
    "triangle-down" = "triangle-down"
  )
  params$point.style <- point.style.map[[tolower(params$point.style)]] %||% params$point.style
  
  # If no existing plot object, create a new highchart object and set axes
  if (is.null(params$plot.object)) {
    PLOT <- highchart()
    
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
    
    # Apply theme
    PLOT <- PLOT |>
      hc_add_theme(hc_thm = params$plot.theme)
    
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
  } else {
    # Use the existing plot object
    PLOT <- params$plot.object
  }
  
  # Generate colors
  NID <- length(unique(data$ID))
  COLORS <- grDevices::hcl.colors(n = NID, palette = params$color.palette)
  
  # Handle fill.polygon for area charts
  if (params$fill.polygon && params$plot.type %in% c("line", "spline")) {
    # Switch to area chart if fill.polygon is TRUE
    params$plot.type <- ifelse(params$plot.type == "line", "area", "areaspline")
  }
  
  # Construct arguments for hc_add_series
  series_args <- list(
    data = data,
    type = params$plot.type,
    hcaes(x = X, y = Y, group = ID),
    colorByPoint = FALSE,
    marker = list(
      enabled = if (params$plot.type == "scatter") TRUE else params$point.marker,
      symbol = params$point.style,    # Use params$point.style directly
      radius = params$point.size
    )
  )
  
  # Include line styling only for appropriate plot types
  if (params$plot.type %in% c("line", "spline", "area", "areaspline")) {
    series_args$dashStyle <- params$line.style  # Use params$line.style directly
    series_args$lineWidth <- params$line.size
  }
  
  # Set fillOpacity for area charts when fill.polygon is TRUE
  if (params$plot.type %in% c("area", "areaspline")) {
    series_args$fillOpacity <- if (params$fill.polygon) 0.5 else 1
  }
  
  # Add series to the plot
  PLOT <- PLOT |>
    do.call(hc_add_series, series_args)
  
  # Tooltip settings
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
  
  # Plot options
  PLOT <- PLOT |>
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = params$point.dataLabels)
      )
    )
  
  # Print max absolute value using annotations
  if (params$print.max.abs) {
    data_abs <- data[, .SD[which.max(abs(Y))], by = ID]
    PLOT <- PLOT |>
      hc_annotations(
        list(
          labels = lapply(1:nrow(data_abs), function(i) {
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
  
  # Plot Save
  if (params$plot.save) {
    htmlwidgets::saveWidget(
      widget = PLOT,
      file = params$plot.filename %||% "plot.html"
    )
  }
  
  # Return the plot
  return(PLOT)
}
# nolint end
