#' Title
#'
#' @param data data.table
#' @param nbins integer
#' @param xAxis.label string
#' @param yAxis.label string
#' @param zAxis.label string
#' @param xAxis.min numeric
#' @param xAxis.max   numeric
#' @param yAxis.min numeric
#' @param yAxis.max numeric  
#' @param axis.fontsize string
#' @param legend.font string
#' @param color.palette palette
#' @param plot.type string
#' @param plot.title string
#' @param title.fontsize string
#' @param title.font string
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly toRGB
#' @import data.table
#' @import grDevices

#' 
#' @import data.table
#' @import grDevices
#'
#' @return plotly object


buildHist2D <- function(data, 
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
  fig <- fig %>% layout(
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
    return(max(nchar(strsplit(as.character(x), "\\.")[[1]][2]), na.rm = TRUE))
  }
}
