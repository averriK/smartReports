
#' Title
#'
#' @param data data.table
#' @param nbins integer
#' @param bin.width numeric
#' @param xAxis.label string
#' @param yAxis.label string
#' @param zAxis.label string
#' @param aspect.ratio list
#' @param xAxis.legend boolean
#' @param yAxis.legend boolean
#' @param zAxis.legend boolean
#' @param xAxis.tickangle numeric
#' @param yAxis.tickangle numeric
#' @param zAxis.tickangle numeric
#' @param axis.fontsize string
#' @param legend.font string
#' @param legend.valign string
#' @param color.palette palette
#' @param caption string
#' @param plot.title string
#' @param title.fontsize string
#' @param title.font string
#' @param smooth.factor integer
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly toRGB
#' @import data.table
#' @import grDevices
#'
#' @return plotly object
buildHist3D <- function(data, 
                   nbins = 15, 
                   bin.width = 0.4, 
                   xAxis.label = "x", 
                   yAxis.label = "y", 
                   zAxis.label = "", 
                   smooth.factor=3,#1 to 5
                   aspect.ratio=list(x = 1, y = 1, z = 1.5),
                   xAxis.legend = TRUE, 
                   yAxis.legend = TRUE, 
                   zAxis.legend = FALSE,
                   xAxis.tickangle = 0,
                   yAxis.tickangle = 0,
                   zAxis.tickangle = 0,
                   axis.fontsize = "14px",
                   legend.font = "Arial",
                   legend.valign = "top",
                   color.palette = "Viridis", 
                   caption = NULL,
                   plot.title = NULL,
                   title.fontsize = "24px",
                   title.font = "Arial") {
  
  # X_bin <- Y_bin <- Z_bin <- Z <- NULL
  DT <- . <-  NULL
 color.scale <- hcl.colors(6, palette = hcl.pals()[6])  
  
  
  # Set axis limits based on the provided data or user input
  Xmin <-min(data$X)
  Xmax <-max(data$X) 
  Ymin <- min(data$Y)
  Ymax <-max(data$Y) 
  
  # Create a copy of the data to avoid modifying the original data table
  DT <- data[X<=Xmax & Y<=Ymax & X>=Xmin & Y>=Ymin,list(X,Y,Z)]
  
  
  
  # Determine decimal places for X and Y data
  x_decimals <- get_decimal_places(DT$X)
  y_decimals <- get_decimal_places(DT$Y)
  # Create bins for X and Y
  BINS <- DT[,list(
    X = cut(X, breaks = seq(Xmin, Xmax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE),
    Y = cut(Y, breaks = seq(Ymin, Ymax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE),
    Z = Z / sum(Z)#Z = Z / max(Z)
  )]

  # Create a matrix for Z values (accumulated probabilities)
  z_mtx <- matrix(0, nrow = nbins, ncol = nbins)
  for (i in 1:nrow(BINS)) {
    z_mtx[BINS$Y[i], BINS$X[i]] <- z_mtx[BINS$Y[i], BINS$X[i]] + BINS$Z[i]
  }
  
  # Define the Z-axis tick values
  Zmin <- min(z_mtx)
  Zmax <- max(z_mtx)
  z_ticks <- seq(Zmin, Zmax, length.out = nbins)
  z_index <- z_mtx  |> as.vector() |> unique() |> sort() 
  NC <- (length(z_index)+6)
  color.scale <- hcl.colors(NC, palette = color.palette)  
  # Draw the 3D histogram
  fig <- plot_ly()

  # Draw the 3D histogram with the updated function
  for (k1 in 1:nrow(z_mtx)) {
    for (k2 in 1:ncol(z_mtx)) {
      z <- z_mtx[k1, k2]
      fig <- add_3Dbar(
        p = fig,
        x = k1,
        y = k2,
        z = z,
        bin.width = bin.width,
        z_index=z_index,
        color.scale = color.scale
      )
    }
  }
  
  # Find the row with the highest Z value
  A <- DT[which.max(Z), .(X, Y, Z)]
  CAPTION <- paste(caption,paste0(xAxis.label,"=",A$X,";",yAxis.label,"=",A$Y),sep="\n")
  

  # Create a list for annotations
  annotations_list <- list()
  
  # Add title as an annotation if provided
  if (!is.null(plot.title)) {
    annotations_list <- append(annotations_list, list(
      list(
        text = plot.title,
        x = 0.5,
        y = 1.1,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = title.fontsize, family = title.font)
      )
    ))
  }
  
  # Add caption as an annotation if provided
  if (!is.null(caption)) {
    annotations_list <- append(annotations_list, list(
      list(
        text = CAPTION,
        x = 0.5,
        y = -0.3,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = axis.fontsize, family = legend.font)
      )
    ))
  }
  
  
  
  
  ticktext_x = seq(Xmin, Xmax, length.out = nbins) |> round(x_decimals)  
  ticktext_y = seq(Ymin, Ymax, length.out = nbins) |> round(y_decimals)
  
  
  
  # Customize the layout for better visualization
  fig <- fig |> layout(
    scene = list(
      xaxis = list(
        title = list(text = xAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the X axis title
        showticklabels = xAxis.legend,
        tickangle = xAxis.tickangle,  # Apply tick angle to X axis ticks
        zeroline = FALSE,
        showgrid = FALSE,  
        tickvals = seq(0, nbins - 1),  # Use 0 to nbins-1 for internal tick positions
        ticktext = ticktext_x,
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for X axis title
        titleangle = 0  # Set angle of X axis title to horizontal
      ),
      yaxis = list(
        title = list(text = yAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the Y axis title
        showticklabels = yAxis.legend,
        tickangle = yAxis.tickangle,  # Apply tick angle to Y axis ticks
        zeroline = FALSE,
        showgrid = FALSE,  
        tickvals = seq(0, nbins - 1),  # Use 0 to nbins-1 for internal tick positions
        ticktext = ticktext_y,
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for Y axis title
        titleangle = 0  # Set angle of Y axis title to horizontal
      ),
      zaxis = list(
        title = list(text = zAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the Z axis title
        showticklabels = zAxis.legend,
        tickangle = zAxis.tickangle,  # Apply tick angle to Z axis ticks
        zeroline = TRUE,
        showgrid = FALSE,  # Turn off grid 
        
        tickvals = z_ticks |> prettyNum(),
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for Z axis title
        titleangle = 0  # Set angle of Z axis title to horizontal
      ),
      aspectmode = "manual",
      aspectratio = aspect.ratio  # Example adjustment
      
    ),
    annotations = annotations_list,
    margin = list(l = 0, r = 0, b = 100, t = 50)
  )
  
  # Display the plot
  return(fig)
}



# Modified add_3Dbar function
add_3Dbar <- function(p, x, y, z, bin.width, z_index, color.scale) {
  w <- bin.width
  color_index <- seq(0,5)+which(z==z_index)
  
  # Create the mesh3d for each column with facecolor in RGB format
  fig <- plotly::add_trace(p, type = "mesh3d",
                           x = c(x - w, x - w, x + w, x + w, x - w, x - w, x + w, x + w),
                           y = c(y - w, y + w, y + w, y - w, y - w, y + w, y + w, y - w),
                           z = c(0, 0, 0, 0, z, z, z, z),
                           i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
                           j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
                           k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
                           facecolor = rep(toRGB(color.scale[color_index]), each = 2),
                           hoverinfo = 'skip')  # Disable hover info to prevent showing non-real values
  
  return(fig)
}

get_decimal_places <- function(x) {
  if (all(x == round(x))) {
    return(0)
  } else {
    return(max(nchar(strsplit(as.character(x), "\\.")[[1]][2]), na.rm = TRUE))
  }
}
