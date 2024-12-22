# Example script demonstrating a combined lines-and-points plot with styles, fills, etc.
# Make sure you have your buildPlot.highchart() function available in the workspace, 
# or adjust the source path accordingly.
# Also, ensure you've installed and/or loaded required packages: data.table, highcharter, etc.
devtools::load_all()
library(data.table)
library(highcharter)
# Force numeric columns to avoid any non-numeric issues
data.lines <- data.table(
  ID    = rep(c("Line A", "Line B", "Line C"), each = 5),
  X     = as.numeric(rep(1:5, 3)),
  Y     = as.numeric(c(seq(1, 5), seq(2, 6), seq(3, 7))),
  #style = c(rep("solid", 5), rep("dotted", 5), rep("dashed", 5)),
  fill  = c(rep(TRUE, 5), rep(TRUE, 5), rep(FALSE, 5))
)

data.points <- data.table(
  ID    = rep(c("Points A", "Points B", "Points C"), each = 4),
  X     = as.numeric(rep(1:4, 3)),
  Y     = as.numeric(c(seq(8, 11), seq(6, 9), seq(4, 7)))
  #style = c(rep("circle", 4), rep("diamond", 4), rep("triangle-down", 4))
)

# Now call your buildPlot.highchart function
# Setting line.type to "spline" for curved lines
# and toggling print.max.abs to show an annotation of max absolute Y for each ID
plot_obj <- buildPlot(Model,
  data.lines      = data.lines,
  data.points     = data.points,
  line.type       = "spline", 
  plot.title      = "Combined Lines and Points Example",
  plot.subtitle   = "Demonstration of fills, line styles, and point styles",
  plot.height     = 500,
  plot.width      = 700,
  xAxis.legend    = "Time (units)",
  yAxis.legend    = "Value",
  print.max.abs   = TRUE
)

# To view the chart interactively in most R environments, just print it:
plot_obj

# Alternatively, you could save the widget to an HTML file:
# htmlwidgets::saveWidget(widget = plot_obj, file = "combinedPlotExample.html")
