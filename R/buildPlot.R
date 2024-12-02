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
