# nolint start
#' Build a model plot
#'
#' This function creates a model plot with data lines and points.
#'
#' @param data.lines A data.table with at least three columns: X (numeric), Y (numeric), and ID (factor).
#' @param data.points A data.table with at least three columns: X (numeric), Y (numeric), and ID (factor).
#' @param xAxis.legend The title for the x-axis.
#' @param yAxis.legend The title for the y-axis.
#' @param line.width The width of the line.
#' @param point.size The size of the points.
#' @param point.shape The shape of the points.
#' @import highcharter
#' @return A highchart object representing the model plot.  
#' 
buildPlot.Model <- function(data.lines, data.points, xAxis.legend, yAxis.legend, 
                         line.width = 1.5, point.size = 2, point.shape = "circle") {
  
  PLOT <- highchart() |>
    hc_add_theme(hc_theme_flat()) |>
    hc_chart(zoomType = "xy") |>
    hc_xAxis(
      type = "logarithmic",
      title = list(text = xAxis.legend),
      lineWidth = line.width,
      lineColor = "black",
      minorGridLineWidth = 0.3,
      gridLineWidth = 0.5,
      minorTickInterval = "auto"
    ) |>
    hc_yAxis(
      type = "logarithmic",
      title = list(text = yAxis.legend),
      lineWidth = line.width,
      lineColor = "black",
      minorGridLineWidth = 0.3,
      gridLineWidth = 0.5,
      minorTickInterval = "auto"
    ) |>
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "middle",
      itemStyle = list(
        fontSize = "12px",
        fontWeight = "normal",
        fontFamily = "Arial"
      )
    )
  
  if (!is.null(data.lines)) {
    PLOT <- PLOT |> 
      hc_add_series(
        data = data.lines,
        type = "spline",
        hcaes(x = X, y = Y, group = ID),
        marker = list(enabled = FALSE),
        lineWidth = 3
      )
  }
  
  if (!is.null(data.points)) {
    PLOT <- PLOT |> 
      hc_add_series(
        data = data.points,
        type = "scatter",
        hcaes(x = X, y = Y, group = ID),
        marker = list(
          radius = point.size,
          symbol = point.shape
        )
      )
  }
  
  return(PLOT)
}

# nolint end
