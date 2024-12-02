#' Build Categorical Plot
#'
#' @param data data.frame
#' @param ... additional parameters
#'
#' @return plot object
#' @export buildPlot.categorical
#'
#' @import data.table
#' @import highcharter
#'
buildPlot.categorical <- function(data, ...) {
  # Extract parameters from the list and assign them to the current environment
  params <- list(...)
  list2env(params, envir = environment())
  
  # Set default theme and color palette
  if (is.null(plot.theme)) {
    plot.theme <- highcharter::hc_theme_flat()
  }
  
  if (is.null(color.palette)) {
    color.palette <- grDevices::hcl.pals()[4]
  }
  
  if (is.null(legend.show)) {
    legend.show <- TRUE
  }
  
  if (is.null(legend.align)) {
    legend.align <- "right"
  }
  
  if (is.null(legend.valign)) {
    legend.valign <- "top"
  }
  
  if (is.null(legend.layout)) {
    legend.layout <- "vertical"
  }
  
  if (is.null(group.legend.fontsize)) {
    group.legend.fontsize <- "12px"
  }
  
  # Initialize plot bands if necessary
  plot_band <- NULL
  
  if (plot.bands == TRUE & "B1" %in% colnames(data)) {
    start_date <- min(data$X[data$B1])
    end_date <- max(data$X[data$B1])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,
      to = as.numeric(as.POSIXct(end_date)) * 1000,
      color = "rgba(230,255,230,0.5)",
      label = list(text = band1.label)
    )
    plot_band <- c(plot_band, list(AUX))
  }
  
  if (plot.bands == TRUE & "B2" %in% colnames(data)) {
    start_date <- min(data$X[data$B2])
    end_date <- max(data$X[data$B2])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,
      to = as.numeric(as.POSIXct(end_date)) * 1000,
      color = "rgba(230,247,255,0.5)",
      label = list(text = band2.label)
    )
    plot_band <- c(plot_band, list(AUX))
  }
  
  # Generate colors for each group
  NID <- length(unique(data$ID))
  COLORS <- grDevices::hcl.colors(n = NID, palette = color.palette)
  data[, IDC := factor(ID, levels = unique(data$ID), labels = COLORS)]
  
  # Initialize the plot
  PLOT <- highcharter::highchart()
  
  PLOT <- PLOT |>
    hc_chart(style = list(fontFamily = "Helvetica")) |>
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    )
  
  # Add stacked columns for each group
  PLOT <- PLOT |>
    hc_add_series(
      data = data,
      type = "column",
      hcaes(x = X, y = Y, group = ID, color = IDC),
      stacking = "normal"
    )
  
  # Customize axes
  PLOT <- PLOT |>
    hc_xAxis(
      type = "category",
      categories = unique(data$X),
      crosshair = TRUE,
      title = list(text = ifelse(!is.null(xAxis.legend), xAxis.legend, "X")),
      plotBands = plot_band
    ) |>
    hc_yAxis(
      type = 'linear', 
      crosshair = TRUE,
      title = list(text = ifelse(!is.null(yAxis.legend), yAxis.legend, "Y"))
    )
  
  # Add plot title and subtitle if provided
  if (!is.null(plot.title)) {
    PLOT <- PLOT |>
      hc_title(text = plot.title)
  }
  
  if (!is.null(plot.subtitle)) {
    PLOT <- PLOT |>
      hc_subtitle(text = plot.subtitle)
  }
  
  # Apply theme
  PLOT <- PLOT |>
    hc_add_theme(plot.theme)
  
  # Enable exporting if specified
  if (!is.null(plot.save) && plot.save) {
    PLOT <- PLOT |>
      hc_exporting(
        enabled = TRUE,
        type = "application/pdf",
        width = 800,
        scale = 3
      )
  }
  
  return(PLOT)
}