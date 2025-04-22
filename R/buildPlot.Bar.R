#' @title Build a bar/column plot (Highcharts)
#' @description Produce a bar/column chart from a data.table that has columns X, Y, and ID.
#'
#' - X can be factor/character for categorical x-axis or numeric for linear x-axis.
#' - Y must be numeric (the bar height).
#' - ID is used to create separate series (groups).
#'
#' @param .data A data.table with columns: X, Y, ID.
#' @param plot.title Character; main chart title.
#' @param plot.subtitle Character; optional subtitle.
#' @param xAxis.legend Character; label for the x-axis. Default "X axis".
#' @param yAxis.legend Character; label for the y-axis. Default "Y axis".
#' @param color.palette Either a character vector of hex colors or a single string
#'   matching a known palette in \code{grDevices::hcl.pals()}. Defaults to "Dark 2".
#' @param legend.show Logical; whether to show the legend. Default TRUE.
#' @param legend.layout One of c("horizontal","vertical"). Default "horizontal".
#' @param legend.align One of c("left","center","right"). Default "right".
#' @param legend.valign One of c("top","middle","bottom"). Default "top".
#' @param plot.width Numeric; width in pixels. If NULL, uses default. 
#' @param plot.height Numeric; height in pixels. If NULL, uses default.
#' @param stacking One of c("none","normal","percent"). Default "none".
#'   - "none": grouped bars
#'   - "normal": stacked bars
#'   - "percent": 100% stacked
#' @param bar.groupPadding Numeric; spacing between groups of bars, default 0.2.
#' @param bar.pointPadding Numeric; spacing between bars within a group, default 0.1.
#' @param bar.borderWidth Numeric; width of bar border line. Default = 0.
#' @param bar.borderColor Character; color of the bar border line. Default "#FFFFFF".
#' @param plot.theme Optional highchart theme object (e.g. \code{hc_theme_flat()}).
#'
#' @return A highchart object representing the bar/column plot.
#'
#' @import data.table
#' @importFrom grDevices hcl.colors hcl.pals
#' @import highcharter
#' @export
buildPlot.Bar <- function(
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
) {
  # Match stacking argument
  stacking <- match.arg(stacking)
  
  # Basic checks
  if (!inherits(.data, "data.table")) {
    stop("`.data` must be a data.table.")
  }
  req_cols <- c("X", "Y", "ID")
  if (!all(req_cols %in% names(.data))) {
    stop("`.data` must have columns: X, Y, ID.")
  }
  if (!is.numeric(.data$Y)) {
    stop("Column 'Y' must be numeric for bar heights.")
  }
  
  # Convert ID to factor if it's character
  if (is.character(.data$ID)) {
    .data[, ID := factor(ID)]
  }
  
  # If X is character, convert to factor so we can manage categories
  if (is.character(.data$X)) {
    .data[, X := factor(X)]
  }
  
  # Unique group levels
  grp_levels <- unique(.data$ID)
  
  # -------------------------------------------------------------------------
  # 1) Build or validate color palette
  # -------------------------------------------------------------------------
  # If user gave multiple hex codes, we just use them (recycled).
  # If user gave one string, we check if it's in hcl.pals().
  process_palette <- function(pal_input, n_needed) {
    if (length(pal_input) > 1) {
      return(pal_input)
    }
    # Single string palette name
    if (!pal_input %in% hcl.pals()) {
      warning("Provided color.palette not in hcl.pals(). Using fallback 'Dark 2'.")
      pal_input <- "Dark 2"
    }
    hcl.colors(n = n_needed, palette = pal_input)
  }
  pal <- process_palette(color.palette, length(grp_levels))
  # Index function
  get_color <- function(i) pal[((i - 1) %% length(pal)) + 1]
  
  # -------------------------------------------------------------------------
  # 2) Initialize highchart object
  # -------------------------------------------------------------------------
  hc <- highchart()
  
  if (!is.null(plot.theme)) {
    hc <- hc |> hc_add_theme(plot.theme)
  } else {
    hc <- hc |> hc_add_theme(hc_theme_flat())
  }
  
  hc <- hc |>
    hc_chart(
      type   = "column",
      width  = if (!is.null(plot.width))  plot.width  else NULL,
      height = if (!is.null(plot.height)) plot.height else NULL,
      style  = list(fontFamily = "Arial")
    )
  
  # Title & subtitle
  if (!is.null(plot.title)) {
    hc <- hc |> hc_title(text = plot.title)
  }
  if (!is.null(plot.subtitle)) {
    hc <- hc |> hc_subtitle(text = plot.subtitle)
  }
  
  # Detect if X is factor or numeric
  x_is_factor <- is.factor(.data$X)
  x_categories <- NULL
  x_type <- "category"
  
  if (!x_is_factor) {
    # numeric axis
    x_type <- "linear"
  } else {
    # factor => use factor levels as categories
    x_categories <- levels(.data$X)
  }
  
  # Configure axes
  hc <- hc |>
    hc_xAxis(
      type       = x_type,
      categories = x_categories,
      title      = list(text = xAxis.legend)
    ) |>
    hc_yAxis(
      title = list(text = yAxis.legend)
    ) |>
    hc_legend(
      enabled = legend.show,
      layout  = legend.layout,
      align   = legend.align,
      verticalAlign = legend.valign
    )
  
  # -------------------------------------------------------------------------
  # 3) Plot Options for column
  # -------------------------------------------------------------------------
  stack_val <- if (stacking == "none") NULL else stacking
  hc <- hc |>
    hc_plotOptions(
      column = list(
        stacking = stack_val,
        borderWidth = bar.borderWidth,
        borderColor = bar.borderColor,
        groupPadding = bar.groupPadding,
        pointPadding = bar.pointPadding
      )
    )
  
  # -------------------------------------------------------------------------
  # 4) Add series per group
  # -------------------------------------------------------------------------
  for (i in seq_along(grp_levels)) {
    grp <- grp_levels[i]
    sub_data <- .data[ID == grp]
    
    if (x_is_factor) {
      # For each factor level, we get Y or default to 0 if missing
      # (since bar charts typically treat missing as 0)
      cat_vec <- numeric(length(x_categories))
      names(cat_vec) <- x_categories
      for (lev in x_categories) {
        v <- sub_data$Y[sub_data$X == lev]
        cat_vec[lev] <- if (length(v)) v[[1]] else 0
      }
      # Add series
      hc <- hc |>
        hc_add_series(
          name = as.character(grp),
          data = as.numeric(cat_vec),
          color = get_color(i),
          type = "column"
        )
    } else {
      # numeric X -> (x,y) pairs
      # highcharter expects a list of (x,y)
      # We can do list_parse2(data.table(x=..., y=...))
      xy_data <- data.table(x = sub_data$X, y = sub_data$Y)
      hc <- hc |>
        hc_add_series(
          name = as.character(grp),
          data = list_parse2(xy_data),
          color = get_color(i),
          type = "column"
        )
    }
  }
  
  return(hc)
}
