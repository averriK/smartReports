#' Title
#'
#' @param hc htmlwidget object
#' @param width numeric
#' @param height numeric
#' @param page_width numeric default 8.27 inches (A4)
#' @param page_height numeric default 11.69 inches (A4)
#' @param factor_width numeric default 1. Full width
#' @param factor_height numeric default 1/2. Half page
#' @param dpi numeric default 96
#' @param filename character default "plot.png"
#' @param delay numeric default 0.5
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot2 webshot

export <- function(hc,width=NULL,height=NULL,page_width=8.27,page_height=11.69,factor_width=1,factor_height=1/2,dpi=96,filename="plot.png",delay=0.5){
  html_file  <- NULL
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(hc, html_file, selfcontained = TRUE)
  if(is.null(width)){width <- (factor_width*page_width * dpi) |> round()}
  if(is.null(height)){height <- (factor_height*page_height * dpi) |> round()}
  webshot2::webshot(html_file, file = filename, vwidth = width, vheight = height,delay=delay)
  unlink(html_file,force=TRUE,recursive = TRUE)
}