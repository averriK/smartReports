#' Title
#'
#' @param data data.frame
#' @param library character c("ggplot2","gg","highcharter","hc","plotly")
#' @param plot.object highcharter object
#' @param plot.title character
#' @param plot.subtitle character
#' @param plot.height numeric
#' @param plot.width numeric
#' @param legend.show boolean
#' @param xAxis.legend character
#' @param yAxis.legend character
#' @param group.legend character
#' @param color.palette character
#' @param plot.type character c("line","spline","point","column","bar")
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
#' @param line.style character c("Solid","Dashed","Dotted", "DashDot","LongDash","LongDashDot","LongDashDotDot")
#' @param plot.theme highcharter object
#' @param point.style character c("circle","square","diamond","triangle","triangle-down")
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
#' @return plot object
#' @export buildPlot
#'
#' @import RColorBrewer
#' @import ggplot2
#' @import epoxy
#' @import ggthemes
#' @import scales
#' @import data.table 
#' @importFrom grDevices hcl.colors 
#' @importFrom grDevices col2rgb
#' @import highcharter 
#' @importFrom epoxy epoxy_html
#' @examples
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot(data=DT)
#' 
#' buildPlot(data=DT,library="highcharter")
#' 
# Main function to build plot with consolidated parameters
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
    plot.type = "line", # c("line","spline","point","column","bar")
    line.style = NULL,
    point.style = 16, # 3:cross 4:plus 5:asterisk 6:circle 7:disk 8:square 9:diamond 10:triangle 11:triangle-down
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
    point.marker=FALSE,
    point.dataLabels=FALSE
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
  # 
  
  # Create a list of parameters to pass to the plotting functions
  params <- list(
    data=data,
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
    point.marker=point.marker,
    point.dataLabels=point.dataLabels
  )
  
  # Switch case to call the respective plot function based on the library parameter
  switch(
    library,
    "ggplot2" = do.call(buildPlot.ggplot2, params),
    "gg" = do.call(buildPlot.ggplot2, params),
    "ggplot" = do.call(buildPlot.ggplot2, params),
    "highcharter" = do.call(buildPlot.highcharter, params),
    "hc" = do.call(buildPlot.highcharter, params),
    "highchart" = do.call(buildPlot.highcharter, params),
    stop("Unsupported library specified")
  )
}


buildPlot.highcharter <- function(data,...) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  # Extract parameters from the list and assign them to the current environment
  
  params <- list(...)
  list2env(params, envir = environment())
  
  
 # browser()
  if(is.null(line.style)){
    line.style <- "Solid"
  }
  
  if(is.null(color.palette)){
    color.palette <- grDevices::hcl.pals()[4]
  }
  
  DATA <- as.data.table(data)[, c("ID", "X", "Y")]
  
  TIP <- epoxy::epoxy_html("{{group.legend}}:{point.series.name}<br> {{xAxis.legend}}={point.x}<br> {{yAxis.legend}}={point.y}")
  COLORS <- grDevices::hcl.colors(n = max(3, min(9,length(unique(DATA$ID)))), palette = color.palette)
  
  if (is.null(plot.object)) {
    PLOT <- highchart()
  } else {
    PLOT <- plot.object
  }
  
  # Prepare data for the shaded region
 
  if (fill.polygon==TRUE && length(unique(DATA$ID)) == 2) {
    DT1 <- DATA[ID == unique(DATA$ID)[1]]
    DT2 <- DATA[ID == unique(DATA$ID)[2]]
    
    # Create polygon points
    polygon_data <- rbind(DT1, DT2[nrow(DT2):1, ], fill=TRUE)
    polygon_data$ID <- fill.group
    
    PLOT <- PLOT |>
      hc_add_series(
        data = polygon_data,
        type = "polygon",
        hcaes(x = X, y = Y),
        name = fill.group,
        color = .hex_to_rgba(COLORS[1], 0.3),
        fillOpacity = 0.3
      )
  }
  
  # c("line","spline","point","column","bar")
  if (tolower(plot.type) %in% c("line", "spline")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA, # main curve
        type = plot.type,
        dashStyle = line.style,
        lineWidth = line.size, # Apply line size here
        
        hcaes(x = X, y = Y, group = ID, color = ID)
      )
  }
  
  if (tolower(plot.type) %in% c("scatter", "point")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA, # main curve
        type = "scatter",
        marker = list(symbol = "circle", radius = point.size), # Apply point size here
        hcaes(x = X, y = Y, group = ID,color = ID)
      )
  }
  
  PLOT <- PLOT |>
    
    hc_yAxis(
      labels = list(enabled = yAxis.label),
      title = list(text = yAxis.legend, style = list(fontSize = yAxis.legend.fontsize)),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE
    ) |>
  
  
    hc_xAxis(
      labels = list(enabled = xAxis.label),
      title = list(text = xAxis.legend, style = list(fontSize = xAxis.legend.fontsize)),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE
    ) |>
    hc_colors(colors = COLORS) |>
    hc_tooltip(
      sort = FALSE,
      split = FALSE,
      crosshairs = TRUE,
      pointFormat = TIP
    ) |>
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) |>
    hc_chart(style = list(fontFamily = "Helvetica"))
  
  if (!is.null(plot.title)) {
    PLOT <- PLOT |>
      hc_title(text = plot.title, fontSize = list(fontSize = plot.title.fontsize))
  }
  
  if (!is.null(plot.subtitle)) {
    PLOT <- PLOT |>
      hc_subtitle(text = plot.subtitle, fontSize = list(fontSize = plot.subtitle.fontsize))
  }
  
  if (!is.na(xAxis.max)) {
    PLOT <- PLOT |>
      hc_xAxis(max = xAxis.max)
  }
  if (!is.na(yAxis.max)) {
    PLOT <- PLOT |>
      hc_yAxis(max = yAxis.max)
  }
  
  if (!is.na(xAxis.min)) {
    PLOT <- PLOT |>
      hc_xAxis(min = xAxis.min)
  }
  if (!is.na(yAxis.min)) {
    PLOT <- PLOT |>
      hc_yAxis(min = yAxis.min)
  }
  if (yAxis.log == TRUE) {
    PLOT <- PLOT |>
      hc_yAxis(type = "logarithmic")
  }
  if (xAxis.log == TRUE) {
    PLOT <- PLOT |>
      hc_xAxis(type = "logarithmic")
  }
  
  if (xAxis.reverse == TRUE) {
    PLOT <- PLOT |>
      hc_xAxis(opposite = TRUE)
  }
  
  if (yAxis.reverse == TRUE) {
    PLOT <- PLOT |>
      hc_yAxis(opposite = TRUE)
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
  
  if(is.null(plot.theme)){
    plot.theme <- highcharter::hc_theme_flat()
  }
  PLOT <- PLOT |>hc_add_theme(hc_thm = plot.theme) 
  
  PLOT <- PLOT |> highcharter::hc_exporting(enabled = plot.save, filename = "hc_plot")
  
  PLOT <- PLOT |>hc_plotOptions(
    series = list(
      dataLabels = list(enabled = point.dataLabels),
      marker = list(enabled = point.marker)
    )
  )
  
  
  return(PLOT)
}

.hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- col2rgb(hex) / 255
  paste0("rgba(", paste(c(rgb, alpha), collapse = ","), ")")
}



buildPlot.ggplot2 <- function(data,...) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  # Extract parameters from the list and assign them to the current environment
  params <- list(...)
  list2env(params, envir = environment())
  
  if (is.null(plot.theme)){
    plot.theme <- theme_flat()
  }
  if (is.null(color.palette)){
    color.palette <- grDevices::hcl.pals()[4]
  }
  
  if (is.null(line.style)){
    line.style <- "solid"
  }
  
  DATA <- as.data.table(data)[, c("ID", "X", "Y")]
  DATA[, ID := as.factor(ID)]  # Convert ID to factor
  
  if (is.null(plot.object)) {
    PLOT <- ggplot(
      data = DATA,
      aes(x = X, y = Y, group = ID, color = ID)
    )
  } else {
    PLOT <- plot.object
    PLOT <- PLOT + geom_blank() # Ensure it accepts additional layers
  }
  PLOT <- PLOT + plot.theme
  
  
  NID <- length(unique(data$ID))
  COLORS <- grDevices::hcl.colors(n=NID, palette = color.palette)
  # COLORS <- grDevices::hcl.colors(n = max(3, min(9, length(unique(DATA$ID)))), palette = color.palette)
  
  if (plot.type == "line") {
    PLOT <- PLOT +
      geom_line(aes(color = ID), size = line.size, linetype = line.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (plot.type == "spline") {
    PLOT <- PLOT +
      geom_smooth(aes(color = ID), method = 'gam', formula = Y ~ s(X, bs = "cs"), size = line.size, linetype = line.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (plot.type == "scatter") {
    PLOT <- PLOT +
      geom_point(aes(color = ID), size = point.size, shape = 16) +
      scale_color_manual(values = COLORS)
  }
  
  if (print.max.abs) {
    max_values <- DATA[, .SD[which.max(abs(Y))], by = ID]
    PLOT <- PLOT +
      geom_point(data = max_values, aes(x = X, y = Y, fill = ID), size = point.size, shape = 16, show.legend = FALSE)
  }
  
  if (!is.na(xAxis.min) || !is.na(xAxis.max)) {
    PLOT <- PLOT + xlim(c(xAxis.min, xAxis.max))
  }
  if (!is.na(yAxis.min) || !is.na(yAxis.max)) {
    PLOT <- PLOT + ylim(c(yAxis.min, yAxis.max))
  }
  
  if (yAxis.log == TRUE) {
    PLOT <- PLOT + scale_y_log10()
  }
  if (xAxis.log == TRUE) {
    PLOT <- PLOT + scale_x_log10()
  }
  
  if (yAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_y_reverse()
  }
  
  if (xAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_x_reverse()
  }
  
  # Ensure plot.title and plot.subtitle are not added if NA
  if (!is.null(plot.title)) {
    PLOT <- PLOT + ggtitle(plot.title)
  }
  
  if (!is.null(plot.subtitle)) {
    PLOT <- PLOT + labs(subtitle = plot.subtitle)
  }
  
  PLOT <- PLOT + xlab(xAxis.legend) + ylab(yAxis.legend)
  
  if (xAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.x = element_blank())
  }
  
  if (yAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.y = element_blank())
  }
  
  # Determine horizontal and vertical positions
  x_position <- switch(legend.align,
                       "left" = 0,
                       "center" = 0.5,
                       "right" = 1)
  
  y_position <- switch(legend.valign,
                       "top" = 1,
                       "middle" = 0.5,
                       "bottom" = 0)
  
  # Determine numeric justification values
  hjust_value <- switch(legend.align,
                        "left" = 0,
                        "center" = 0.5,
                        "right" = 1)
  
  vjust_value <- switch(legend.valign,
                        "top" = 1,
                        "middle" = 0.5,
                        "bottom" = 0)
  
  # Applying the legend settings to the plot
  if (legend.show) {
    PLOT <- PLOT + theme(
      legend.position = c(x_position, y_position),
      legend.direction = legend.layout,
      legend.justification = c(hjust_value, vjust_value)
    )
  } else {
    PLOT <- PLOT + theme(legend.position = "none")
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
