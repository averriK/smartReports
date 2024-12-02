#' Title
#' @param data data.frame
#' @param ... extra arguments
#' 
#' @return highcharter object
#'
#' @import data.table 
#' @importFrom grDevices hcl.colors 
#' @importFrom grDevices col2rgb
#' @import highcharter 
#' @importFrom epoxy epoxy_html
#'


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

