#' Build 2D histogram or contour plot with plotly
#'
#' Creates a 2D heatmap or contour plot from data with X, Y, and Z columns.
#' Z values are accumulated into 2D bins defined by nbins parameter.
#'
#' @param data data.table with columns X, Y, Z
#' @param nbins integer, number of bins in each dimension (default 30)
#' @param xAxis.label string, x-axis label
#' @param yAxis.label string, y-axis label
#' @param zAxis.label string, colorbar label
#' @param xAxis.min numeric, minimum x value (default: min(data$X))
#' @param xAxis.max numeric, maximum x value (default: max(data$X))
#' @param yAxis.min numeric, minimum y value (default: min(data$Y))
#' @param yAxis.max numeric, maximum y value (default: max(data$Y))
#' @param axis.fontsize string, axis label font size (default "14px")
#' @param legend.font string, axis font family (default "Arial")
#' @param color.palette string, color palette name from \code{grDevices::hcl.pals()} (default: 33rd palette)
#' @param plot.type string, either "heatmap" or "contour"
#' @param plot.title string, plot title (optional)
#' @param title.fontsize string, title font size (default "24px")
#' @param title.font string, title font family (default "Arial")
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly toRGB
#' @import data.table
#' @import grDevices
#'
#' @return plotly object
#' @export


buildPlot.Hist2D <- function(data,
                   nbins = 30, 
                   xAxis.label = "x", 
                   yAxis.label = "y", 
                   zAxis.label = "z", 
                   xAxis.min = NULL, 
                   xAxis.max = NULL, 
                   yAxis.min = NULL, 
                   yAxis.max = NULL, 
                   axis.fontsize = "14px",
                   legend.font = "Arial",
                   color.palette = hcl.pals()[33], 
                   plot.type = c("heatmap", "contour"),
                   plot.title = NULL,
                   title.fontsize = "24px",
                   title.font = "Arial") {
  
  X_bin <- Y_bin <- . <-  NULL
  
  # Create a copy of the data to avoid modifying the original data table
  DT <- copy(data)
  
  # Set default color palette if none is provided
  
  color.scale <- hcl.colors(nbins, palette = color.palette)  
  
  # Set axis limits based on the provided data or user input
  Xmin <- if (!is.null(xAxis.min)) xAxis.min else min(data$X)
  Xmax <- if (!is.null(xAxis.max)) xAxis.max else max(data$X) + 1e-10
  Ymin <- if (!is.null(yAxis.min)) yAxis.min else min(data$Y)
  Ymax <- if (!is.null(yAxis.max)) yAxis.max else max(data$Y) + 1e-10
  
  # Create bins for X and Y
  DT[, X_bin := cut(X, breaks = seq(Xmin, Xmax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE)]
  DT[, Y_bin := cut(Y, breaks = seq(Ymin, Ymax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE)]
  
  # Create a matrix for Z values (accumulated probabilities)
  z_mtx <- matrix(0, nrow = nbins, ncol = nbins)
  for (i in 1:nrow(DT)) {
    z_mtx[DT$Y_bin[i], DT$X_bin[i]] <- z_mtx[DT$Y_bin[i], DT$X_bin[i]] + DT$Z[i]
  }
  
  # Prepare data for Plotly
  x_vals <- seq(Xmin, Xmax, length.out = nbins)
  y_vals <- seq(Ymin, Ymax, length.out = nbins)
  
  # Define the number of decimal places for X and Y
  x_decimals <- get_decimal_places(DT$X)
  y_decimals <- get_decimal_places(DT$Y)
  
  # Create the 2D plot based on the chosen plot type
  if (plot.type == "heatmap") {
    fig <- plot_ly(
      x = round(x_vals, x_decimals),
      y = round(y_vals, y_decimals),
      z = z_mtx,
      type = "heatmap",
      # colorscale = list(
      #   cbind(seq(0, 1, length.out = length(color.scale)), color.scale)
      # ),
      colors=color.scale,
      colorbar = list(title = zAxis.label)
    )
  } else if (plot.type == "contour") {
    fig <- plot_ly(
      x = round(x_vals, x_decimals),
      y = round(y_vals, y_decimals),
      z = z_mtx,
      type = "contour",
      # colorscale = list(
      #   cbind(seq(0, 1, length.out = length(color.scale)), color.scale)
      # ),
      colors=color.scale,
      colorbar = list(title = zAxis.label)
    )
  }
  
  # Customize the layout
  fig <- fig |> layout(
    title = list(text = plot.title, font = list(size = title.fontsize, family = title.font)),
    xaxis = list(title = xAxis.label, tickvals = round(x_vals, x_decimals), tickformat = paste0(".", x_decimals, "f"), titlefont = list(size = axis.fontsize, family = legend.font)),
    yaxis = list(title = yAxis.label, tickvals = round(y_vals, y_decimals), tickformat = paste0(".", y_decimals, "f"), titlefont = list(size = axis.fontsize, family = legend.font)),
    margin = list(l = 0, r = 0, b = 100, t = 50)
  )
  
  # Display the plot
  return(fig)
}


get_decimal_places <- function(x) {
  if (all(x == round(x))) {
    return(0)
  } else {
    decimals <- sapply(strsplit(as.character(x), "\\."), function(parts) {
      if (length(parts) == 2) nchar(parts[2]) else 0
    })
    return(max(decimals, na.rm = TRUE))
  }
}
