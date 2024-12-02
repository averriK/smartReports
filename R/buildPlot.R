#' Title
#'
#' @param data data.frame
#' @param library character c("ggplot2","gg","highcharter","hc","plotly")
#' @param plot.object highcharter or ggplot2 object
#' @param plot.title character
#' @param plot.subtitle character
#' @param plot.height numeric
#' @param plot.width numeric
#' @param legend.show boolean
#' @param xAxis.legend character
#' @param yAxis.legend character
#' @param group.legend character
#' @param color.palette character
#' @param plot.type character c("line","spline","point","scatter","column","bar")
#' @param xAxis.log boolean
#' @param yAxis.log boolean
#' @param xAxis.reverse boolean
#' @param yAxis.reverse boolean
#' @param line.size numeric
#' @param point.size numeric
#' @param xAxis.max numeric
#' @param yAxis.max numeric
#' @param xAxis.min numeric
#' @param yAxis.min numeric
#' @param xAxis.label boolean
#' @param yAxis.label boolean
#' @param legend.layout character
#' @param legend.align character c("center", "left", "right")
#' @param legend.valign character c("top", "middle", "bottom")
#' @param line.style character
#' @param plot.theme highcharter or ggplot2 theme object
#' @param point.style character or numeric
#' @param plot.save boolean
#' @param fill.polygon boolean
#' @param fill.group character
#' @param xAxis.legend.fontsize character
#' @param yAxis.legend.fontsize character
#' @param group.legend.fontsize character
#' @param plot.title.fontsize character
#' @param plot.subtitle.fontsize character
#' @param print.max.abs boolean
#' @param point.marker boolean
#' @param point.dataLabels boolean
#'
#' @import ggplot2
#' @import highcharter
#' @return plot object
#' @export buildPlot
#'
#' @examples
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot(data=DT)
#' 
#' buildPlot(data=DT,library="highcharter")
#' 
buildPlot <- function(
    data,
    library = "highcharter",
    plot.object = NULL,
    plot.title = NULL,
    plot.subtitle = NULL,
    plot.height = NULL,
    plot.width = NULL,
    xAxis.legend = "X",
    yAxis.legend = "Y",
    group.legend = "ID",
    color.palette = NULL,
    plot.type = "line", # c("line","spline","point","scatter","column","bar")
    line.style = "solid",
    point.style = NULL,
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
    legend.valign = "top", # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = TRUE,
    plot.theme = NULL,
    xAxis.legend.fontsize = "14px",
    yAxis.legend.fontsize = "14px",
    group.legend.fontsize = "12px",
    plot.title.fontsize = "24px",
    plot.subtitle.fontsize = "18px",
    print.max.abs = FALSE, # New flag for printing max absolute values
    fill.polygon = FALSE,
    fill.group = "",
    point.marker = FALSE,
    point.dataLabels = FALSE
) {
  # Validate legend.valign
  valid_valign <- c("top", "middle", "bottom")
  valid_align <- c("left", "center", "right")
  valid_layout <- c("horizontal", "vertical")
  if (!(legend.valign %in% valid_valign)) {
    stop(paste("Invalid legend.valign:", legend.valign, "Expected one of:", paste(valid_valign, collapse = ", ")))
  }
  
  # Validate legend.align
  if (!(legend.align %in% valid_align)) {
    stop(paste("Invalid legend.align:", legend.align, "Expected one of:", paste(valid_align, collapse = ", ")))
  }
  
  # Validate legend.layout
  if (!(legend.layout %in% valid_layout)) {
    stop(paste("Invalid legend.layout:", legend.layout, "Expected one of:", paste(valid_layout, collapse = ", ")))
  }
  
  # Create a list of parameters to pass to the plotting functions
  params <- list(
    data = data,
    plot.object = plot.object,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    plot.height = plot.height,
    plot.width = plot.width,
    xAxis.legend = xAxis.legend,
    yAxis.legend = yAxis.legend,
    group.legend = group.legend,
    color.palette = color.palette,
    plot.type = plot.type,
    line.style = line.style,
    point.style = point.style,
    line.size = line.size,
    point.size = point.size,
    xAxis.log = xAxis.log,
    yAxis.log = yAxis.log,
    xAxis.reverse = xAxis.reverse,
    yAxis.reverse = yAxis.reverse,
    xAxis.max = xAxis.max,
    yAxis.max = yAxis.max,
    xAxis.min = xAxis.min,
    yAxis.min = yAxis.min,
    xAxis.label = xAxis.label,
    yAxis.label = yAxis.label,
    legend.layout = legend.layout,
    legend.align = legend.align,
    legend.valign = legend.valign,
    legend.show = legend.show,
    plot.save = plot.save,
    plot.theme = plot.theme,
    xAxis.legend.fontsize = xAxis.legend.fontsize,
    yAxis.legend.fontsize = yAxis.legend.fontsize,
    group.legend.fontsize = group.legend.fontsize,
    plot.title.fontsize = plot.title.fontsize,
    plot.subtitle.fontsize = plot.subtitle.fontsize,
    print.max.abs = print.max.abs,
    fill.polygon = fill.polygon,
    fill.group = fill.group,
    point.marker = point.marker,
    point.dataLabels = point.dataLabels
  )
  
  # Switch case to call the respective plot function based on the library parameter
  switch(
    library,
    "ggplot2" = do.call(.build.ggplot2, params),
    "gg" = do.call(.build.ggplot2, params),
    "ggplot" = do.call(.build.ggplot2, params),
    "highcharter" = do.call(.build.highcharter, params),
    "hc" = do.call(.build.highcharter, params),
    "highchart" = do.call(.build.highcharter, params),
    stop("Unsupported library specified")
  )
}



.build.highcharter <- function(data, ...) {
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }

  # Extract parameters from the list and assign them to the current environment
  params <- list(...)
  list2env(params, envir = environment())

  ## **A. Line Styles Mapping**
  # Mapping user-friendly line style names to Highcharts dashStyles
  line_style_mapping <- c(
    "solid" = "Solid",
    "dashed" = "Dash",
    "dotted" = "Dot",
    "dashdot" = "DashDot",
    "longdash" = "LongDash",
    "longdashdot" = "LongDashDot",
    "shortdash" = "ShortDash",
    "shortdashdot" = "ShortDashDot",
    "shortdot" = "ShortDot"
  )
  if (is.null(line.style)) {
    line.style <- "Solid"
  } else {
    line.style <- line_style_mapping[tolower(line.style)]
    if (is.na(line.style)) {
      line.style <- "Solid"  # Default to Solid if not found
    }
  }

  ## **B. Point Styles Mapping**
  # Mapping common point style names to Highcharts symbols
  if (is.null(point.style)) {
    point.style <- "circle"  # Default to circle
  }
  point_style_mapping <- c(
    "circle" = "circle",
    "square" = "square",
    "diamond" = "diamond",
    "triangle" = "triangle",
    "triangle-down" = "triangle-down",
    "triangle-up" = "triangle",
    "triangle-left" = "triangle-left",
    "triangle-right" = "triangle-right",
    "cross" = "cross",
    "plus" = "plus"
  )
  if (is.character(point.style)) {
    point.style <- point_style_mapping[tolower(point.style)]
    if (is.na(point.style)) {
      point.style <- "circle"  # Default to circle if not found
    }
  }

  ## **C. Font Sizes Handling**
  # Ensure font sizes are specified as character strings with units (e.g., "14px")
  ensure_font_size_unit <- function(font_size) {
    if (is.numeric(font_size)) {
      paste0(font_size, "px")
    } else {
      font_size
    }
  }
  xAxis.legend.fontsize <- ensure_font_size_unit(xAxis.legend.fontsize)
  yAxis.legend.fontsize <- ensure_font_size_unit(yAxis.legend.fontsize)
  group.legend.fontsize <- ensure_font_size_unit(group.legend.fontsize)
  plot.title.fontsize <- ensure_font_size_unit(plot.title.fontsize)
  plot.subtitle.fontsize <- ensure_font_size_unit(plot.subtitle.fontsize)

  if (is.null(color.palette)) {
    color.palette <- grDevices::hcl.pals()[4]
  }

  DATA <- data.table::as.data.table(data)[, c("ID", "X", "Y")]

  TIP <- epoxy::epoxy_html("{{group.legend}}: {point.series.name}<br>{{xAxis.legend}}: {point.x}<br>{{yAxis.legend}}: {point.y}")
  
  # Ensure a minimum number of colors to prevent hcl.colors() from failing
  num_colors <- max(3, min(9, length(unique(DATA$ID))))
  COLORS <- grDevices::hcl.colors(n = num_colors, palette = color.palette)

  if (is.null(plot.object)) {
    PLOT <- highchart()
  } else {
    PLOT <- plot.object
  }

  # Prepare data for the shaded region
  if (fill.polygon == TRUE && length(unique(DATA$ID)) == 2) {
    DT1 <- DATA[ID == unique(DATA$ID)[1]]
    DT2 <- DATA[ID == unique(DATA$ID)[2]]

    # Create polygon points
    polygon_data <- rbind(DT1, DT2[nrow(DT2):1, ], fill = TRUE)
    polygon_data$ID <- fill.group

    PLOT <- PLOT |>
      hc_add_series(
        data = polygon_data,
        type = "polygon",
        hcaes(x = X, y = Y),
        name = fill.group,
        color = .hex_to_rgba(COLORS[1], 0.3),
        fillOpacity = 0.3,
        enableMouseTracking = FALSE
      )
  }

  # Plot types
  if (tolower(plot.type) %in% c("line", "spline")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA,
        type = plot.type,
        dashStyle = line.style,
        lineWidth = line.size,
        marker = list(enabled = point.marker),
        hcaes(x = X, y = Y, group = ID, color = ID)
      )
  }

  if (tolower(plot.type) %in% c("scatter", "point")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA,
        type = "scatter",
        marker = list(symbol = point.style, radius = point.size),
        hcaes(x = X, y = Y, group = ID, color = ID)
      )
  }

  PLOT <- PLOT |>
    hc_yAxis(
      labels = list(enabled = yAxis.label),
      title = list(text = yAxis.legend, style = list(fontSize = yAxis.legend.fontsize)),
      reversed = yAxis.reverse,
      max = ifelse(!is.na(yAxis.max), yAxis.max, NULL),
      min = ifelse(!is.na(yAxis.min), yAxis.min, NULL),
      type = ifelse(yAxis.log, "logarithmic", "linear")
    ) |>
    hc_xAxis(
      labels = list(enabled = xAxis.label),
      title = list(text = xAxis.legend, style = list(fontSize = xAxis.legend.fontsize)),
      reversed = xAxis.reverse,
      max = ifelse(!is.na(xAxis.max), xAxis.max, NULL),
      min = ifelse(!is.na(xAxis.min), xAxis.min, NULL),
      type = ifelse(xAxis.log, "logarithmic", "linear")
    ) |>
    hc_colors(colors = COLORS) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = TIP
    ) |>
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize),
      title = list(text = group.legend)
    ) |>
    hc_chart(style = list(fontFamily = "Helvetica"))

  if (!is.null(plot.title)) {
    PLOT <- PLOT |>
      hc_title(text = plot.title, style = list(fontSize = plot.title.fontsize))
  }

  if (!is.null(plot.subtitle)) {
    PLOT <- PLOT |>
      hc_subtitle(text = plot.subtitle, style = list(fontSize = plot.subtitle.fontsize))
  }

  if (!is.null(plot.height) & is.null(plot.width)) {
    PLOT <- PLOT |> hc_size(height = plot.height)
  }

  if (!is.null(plot.width) & is.null(plot.height)) {
    PLOT <- PLOT |> hc_size(width = plot.width)
  }

  if (!is.null(plot.width) & !is.null(plot.height)) {
    PLOT <- PLOT |> hc_size(width = plot.width, height = plot.height)
  }

  # Apply custom theme
  if (is.null(plot.theme)) {
    plot.theme <- highcharter::hc_theme_flat()
  }
  PLOT <- PLOT |> hc_add_theme(hc_thm = plot.theme)

  ## **E. Plot Saving Functionality**
  if (plot.save == TRUE) {
    PLOT <- PLOT |>
      hc_exporting(
        enabled = TRUE,
        filename = "hc_plot",
        buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")))
      )
  } else {
    PLOT <- PLOT |> hc_exporting(enabled = FALSE)
  }

  ## **G. Data Labels and Marker Visibility**
  PLOT <- PLOT |> hc_plotOptions(
    series = list(
      dataLabels = list(enabled = point.dataLabels),
      marker = list(enabled = point.marker)
    )
  )

  return(PLOT)
}

# Helper function to convert hex to rgba
.hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- grDevices::col2rgb(hex) / 255
  paste0("rgba(", paste(c(rgb, alpha), collapse = ","), ")")
}



.build.ggplot2 <- function(data, ...) {
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  
  # Extract parameters from the list and assign them to the current environment
  params <- list(...)
  list2env(params, envir = environment())
  
  ## **A. Line Styles Mapping**
  # Mapping user-friendly line style names to ggplot2 linetypes
  line_style_mapping <- c(
    "solid" = "solid",
    "dashed" = "dashed",
    "dotted" = "dotted",
    "dotdash" = "dotdash",
    "longdash" = "longdash",
    "twodash" = "twodash"
  )
  if (is.null(line.style)) {
    line.style <- "solid"
  } else {
    line.style <- line_style_mapping[tolower(line.style)]
    if (is.na(line.style)) {
      line.style <- "solid"  # Default to solid if not found
    }
  }
  
  ## **B. Point Styles Mapping**
  # Mapping common point style names to ggplot2 shape numbers
  if (is.null(point.style)) {
    point.style <- 16  # Default to circle
  }
  point_style_mapping <- c(
    "circle" = 16,
    "square" = 15,
    "diamond" = 18,
    "triangle" = 17,
    "triangle-down" = 25,
    "plus" = 3,
    "cross" = 4,
    "asterisk" = 8
  )
  if (is.character(point.style)) {
    point.style <- point_style_mapping[tolower(point.style)]
    if (is.na(point.style)) {
      point.style <- 16  # Default to circle if not found
    }
  }
  
  ## **C. Font Sizes Handling**
  # Function to convert font size strings to numeric values
  font_size_to_numeric <- function(font_size) {
    as.numeric(gsub("[^0-9.]", "", font_size))
  }
  
  if (is.null(plot.theme)) {
    plot.theme <- theme_flat()
  }
  
  if (is.null(color.palette)) {
    color.palette <- grDevices::hcl.pals()[4]
  }
  
  DATA <- data.table::as.data.table(data)[, c("ID", "X", "Y")]
  DATA[, ID := as.factor(ID)]  # Convert ID to factor
  
  if (is.null(plot.object)) {
    PLOT <- ggplot(
      data = DATA,
      aes(x = X, y = Y, group = ID, color = ID)
    )
  } else {
    PLOT <- plot.object
    PLOT <- PLOT + geom_blank()  # Ensure it accepts additional layers
  }
  PLOT <- PLOT + plot.theme
  
  NID <- length(unique(DATA$ID))
  COLORS <- grDevices::hcl.colors(n = NID, palette = color.palette)
  
  ## **Plot Types**
  if (tolower(plot.type) == "line") {
    PLOT <- PLOT +
      geom_line(size = line.size, linetype = line.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (tolower(plot.type) == "spline") {
    PLOT <- PLOT +
      geom_smooth(
        method = 'gam',
        formula = y ~ s(x, bs = "cs"),
        size = line.size,
        linetype = line.style,
        se = FALSE
      ) +
      scale_color_manual(values = COLORS)
  }
  
  if (tolower(plot.type) %in% c("scatter", "point")) {
    PLOT <- PLOT +
      geom_point(size = point.size, shape = point.style) +
      scale_color_manual(values = COLORS)
  }
  
  ## **G. Tooltip and Data Labels**
  if (point.dataLabels == TRUE) {
    PLOT <- PLOT + geom_text(
      aes(label = paste(yAxis.legend, "=", Y)),
      vjust = -0.5,
      size = point.size * 0.8
    )
  }
  
  if (print.max.abs) {
    max_values <- DATA[, .SD[which.max(abs(Y))], by = ID]
    PLOT <- PLOT +
      geom_point(
        data = max_values,
        aes(x = X, y = Y),
        size = point.size,
        shape = point.style,
        show.legend = FALSE
      )
  }
  
  ## **Axis Limits**
  if (!is.na(xAxis.min) || !is.na(xAxis.max)) {
    PLOT <- PLOT + coord_cartesian(xlim = c(xAxis.min, xAxis.max))
  }
  
  if (!is.na(yAxis.min) || !is.na(yAxis.max)) {
    PLOT <- PLOT + coord_cartesian(ylim = c(yAxis.min, yAxis.max))
  }
  
  ## **Axis Transformations**
  if (xAxis.log == TRUE) {
    PLOT <- PLOT + scale_x_log10()
  }
  
  if (yAxis.log == TRUE) {
    PLOT <- PLOT + scale_y_log10()
  }
  
  if (xAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_x_reverse()
  }
  
  if (yAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_y_reverse()
  }
  
  ## **C. Apply Font Sizes**
  PLOT <- PLOT + theme(
    axis.title.x = element_text(size = font_size_to_numeric(xAxis.legend.fontsize)),
    axis.title.y = element_text(size = font_size_to_numeric(yAxis.legend.fontsize)),
    legend.text = element_text(size = font_size_to_numeric(group.legend.fontsize)),
    plot.title = element_text(size = font_size_to_numeric(plot.title.fontsize)),
    plot.subtitle = element_text(size = font_size_to_numeric(plot.subtitle.fontsize))
  )
  
  ## **Plot Titles and Labels**
  PLOT <- PLOT +
    labs(
      title = plot.title,
      subtitle = plot.subtitle,
      x = xAxis.legend,
      y = yAxis.legend,
      color = group.legend
    )
  
  ## **Control Axis Labels Display**
  if (!xAxis.label) {
    PLOT <- PLOT + theme(axis.title.x = element_blank())
  }
  
  if (!yAxis.label) {
    PLOT <- PLOT + theme(axis.title.y = element_blank())
  }
  
  ## **D. Legend Positioning**
  # Mapping legend positions
  position_mapping <- function(align, valign) {
    x_mapping <- c("left" = 0, "center" = 0.5, "right" = 1)
    y_mapping <- c("bottom" = 0, "middle" = 0.5, "top" = 1)
    return(c(x_mapping[align], y_mapping[valign]))
  }
  justification_mapping <- function(align, valign) {
    x_mapping <- c("left" = 0, "center" = 0.5, "right" = 1)
    y_mapping <- c("bottom" = 0, "middle" = 0.5, "top" = 1)
    return(c(x_mapping[align], y_mapping[valign]))
  }
  legend_pos <- position_mapping(legend.align, legend.valign)
  legend_just <- justification_mapping(legend.align, legend.valign)
  
  if (legend.show) {
    PLOT <- PLOT + theme(
      legend.position = legend_pos,
      legend.justification = legend_just,
      legend.direction = legend.layout
    )
  } else {
    PLOT <- PLOT + theme(legend.position = "none")
  }
  
  ## **E. Plot Saving Functionality**
  if (plot.save == TRUE) {
    filename <- "ggplot2_plot.png"
    ggsave(filename = filename, plot = PLOT, width = plot.width, height = plot.height)
  }
  
  return(PLOT)
}

theme_flat <- function() {
  theme_light() +
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
