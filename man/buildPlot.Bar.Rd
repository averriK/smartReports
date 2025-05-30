% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildPlot.Bar.R
\name{buildPlot.Bar}
\alias{buildPlot.Bar}
\title{Build a bar/column plot (Highcharts)}
\usage{
buildPlot.Bar(
  .data,
  plot.title = NULL,
  plot.subtitle = NULL,
  xAxis.legend = "X axis",
  yAxis.legend = "Y axis",
  color.palette = "Dark 2",
  legend.show = TRUE,
  legend.layout = "horizontal",
  legend.align = "right",
  legend.valign = "top",
  plot.width = NULL,
  plot.height = NULL,
  stacking = c("none", "normal", "percent"),
  bar.groupPadding = 0.2,
  bar.pointPadding = 0.1,
  bar.borderWidth = 0,
  bar.borderColor = "#FFFFFF",
  plot.theme = NULL
)
}
\arguments{
\item{.data}{A data.table with columns: X, Y, ID.}

\item{plot.title}{Character; main chart title.}

\item{plot.subtitle}{Character; optional subtitle.}

\item{xAxis.legend}{Character; label for the x-axis. Default "X axis".}

\item{yAxis.legend}{Character; label for the y-axis. Default "Y axis".}

\item{color.palette}{Either a character vector of hex colors or a single string
matching a known palette in \code{grDevices::hcl.pals()}. Defaults to "Dark 2".}

\item{legend.show}{Logical; whether to show the legend. Default TRUE.}

\item{legend.layout}{One of c("horizontal","vertical"). Default "horizontal".}

\item{legend.align}{One of c("left","center","right"). Default "right".}

\item{legend.valign}{One of c("top","middle","bottom"). Default "top".}

\item{plot.width}{Numeric; width in pixels. If NULL, uses default.}

\item{plot.height}{Numeric; height in pixels. If NULL, uses default.}

\item{stacking}{One of c("none","normal","percent"). Default "none".
\itemize{
\item "none": grouped bars
\item "normal": stacked bars
\item "percent": 100\% stacked
}}

\item{bar.groupPadding}{Numeric; spacing between groups of bars, default 0.2.}

\item{bar.pointPadding}{Numeric; spacing between bars within a group, default 0.1.}

\item{bar.borderWidth}{Numeric; width of bar border line. Default = 0.}

\item{bar.borderColor}{Character; color of the bar border line. Default "#FFFFFF".}

\item{plot.theme}{Optional highchart theme object (e.g. \code{hc_theme_flat()}).}
}
\value{
A highchart object representing the bar/column plot.
}
\description{
Produce a bar/column chart from a data.table that has columns X, Y, and ID.
\itemize{
\item X can be factor/character for categorical x-axis or numeric for linear x-axis.
\item Y must be numeric (the bar height).
\item ID is used to create separate series (groups).
}
}
