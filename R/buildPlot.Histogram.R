#' Build a histogram (and/or density) plot
#'
#' Creates a histogram, density plot, or both, with optional log-scale and quantile markers.
#'
#' @param .data A data.table with columns "X" (numeric) and "ID" (factor or character).
#' @param plot.type Character vector: \code{"histogram"}, \code{"density"}, or \code{c("histogram","density")}.
#' @param histogram.breaks Either NULL (automatic Freedman–Diaconis) or numeric (number of bins).
#' @param xAxis.log Logical; if TRUE, use log-scale for the X data. Default FALSE.
#' @param xAxis.offset Numeric; fraction to expand the x-axis range on each side. Default 0.1.
#' @param xAxis.legend Character; label to show on the x-axis. Default "X axis".
#' @param yAxis.legend Character; label to show on the y-axis. Default "Count".
#' @param plot.title  Character; main title. Default NULL.
#' @param plot.subtitle Character; optional subtitle. Default NULL.
#' @param plot.markers Logical; if TRUE, add quantile markers. Default FALSE.
#' @param color.palette Either a character vector of colors or a single string (see details). 
#' @param legend.layout One of \code{c("horizontal","vertical")}. Default "horizontal".
#' @param legend.align One of \code{c("left","center","right")}. Default "right".
#' @param legend.valign One of \code{c("top","middle","bottom")}. Default "top".
#' @param legend.show Logical; if FALSE, hides the legend. Default TRUE.
#' @param plot.height Numeric, chart height in pixels (if NULL, defaults). Default NULL.
#' @param plot.width Numeric, chart width in pixels (if NULL, defaults). Default NULL.
#' @import data.table
#' @importFrom stats quantile density IQR na.omit
#' @importFrom graphics hist
#' @import highcharter
#' @return A highchart object.
#' 
#' @export
buildPlot.Histogram <- function(
    .data,
    plot.type = c("histogram"),
    histogram.breaks = NULL,
    xAxis.log = FALSE,
    xAxis.offset = 0.1,
    xAxis.legend = "X axis",
    yAxis.legend = "Count",
    plot.title = NULL,
    plot.subtitle = NULL,
    plot.markers = FALSE,
    color.palette = "Dark3",
    legend.layout = "horizontal",
    legend.align = "right",
    legend.valign = "top",
    legend.show = TRUE,
    plot.height = NULL,
    plot.width = NULL
) {
  # ------------------------------------------------------------------------
  # 1) Validate the data
  # ------------------------------------------------------------------------
  plot.type <- match.arg(plot.type, choices = c("histogram", "density"), several.ok = TRUE)
  if (!inherits(.data, "data.table")) {
    stop("`.data` must be a data.table.")
  }
  # Must have columns "X" and "ID"
  req_cols <- c("X", "ID")
  if (!all(req_cols %in% names(.data))) {
    stop("`.data` must have columns named 'X' (numeric) and 'ID' (factor or character).")
  }
  if (!is.numeric(.data$X)) {
    stop("Column 'X' must be numeric.")
  }
  if (!is.factor(.data$ID) && !is.character(.data$ID)) {
    stop("Column 'ID' should be factor or character.")
  }
  
  # Ensure ID is factor (optional, but often helpful for plotting)
  if (is.character(.data$ID)) {
    .data[, ID := factor(ID)]
  }
  
  .groups <- unique(.data$ID)
  
  # ------------------------------------------------------------------------
  # 2) Color palette handling
  # ------------------------------------------------------------------------
  process_palette <- function(pal_input, n_needed) {
    # If the user supplies multiple hex codes, use them directly:
    if (length(pal_input) > 1) {
      return(pal_input)
    }
    # Otherwise, fallback or interpret a single string
    # If you want advanced palette logic (RColorBrewer, etc.), do it here:
    fallback <- c("#4878D0", "#EE854A", "#6ACC64", "#D65F5F", "#956CB4", 
                  "#82C6E2", "#D7B5A6", "#8B8B8B", "#B5DE2B", "#82C6E2")
    return(fallback)
  }
  pal <- process_palette(color.palette, length(.groups))
  get_color <- function(i) pal[((i - 1) %% length(pal)) + 1]
  
  # ------------------------------------------------------------------------
  # 3) Determine common breaks for histogram-based scaling
  #    (We need this even if plot.type="density" only, because
  #     we scale densities by the histogram's max count.)
  # ------------------------------------------------------------------------
  # Filter out non-positive if log-scale
  if (xAxis.log) {
    x_positive <- .data$X[.data$X > 0]
    if (!length(x_positive)) {
      stop("No positive values in 'X' for log-scale.")
    }
    if (is.null(histogram.breaks)) {
      # Freedman–Diaconis in log10
      logX <- log10(x_positive)
      iqr_logX <- IQR(logX, na.rm = TRUE)
      n <- length(na.omit(logX))
      bin_width <- 2 * iqr_logX / (n^(1/3))
      if (bin_width <= 0) {
        bin_width <- (max(logX) - min(logX)) / 30
      }
      num_bins <- max(1, floor((max(logX) - min(logX)) / bin_width))
      common_log_breaks <- seq(min(logX), max(logX), length.out = num_bins + 1)
      common_breaks <- 10^common_log_breaks
    } else {
      all_range <- range(x_positive, na.rm = TRUE)
      common_breaks <- 10^seq(log10(all_range[1]), log10(all_range[2]),
                              length.out = histogram.breaks + 1)
    }
  } else {
    all_range <- range(.data$X, na.rm = TRUE)
    if (is.null(histogram.breaks)) {
      x_all <- na.omit(.data$X)
      iqr_x <- IQR(x_all)
      n <- length(x_all)
      bin_width <- 2 * iqr_x / (n^(1/3))
      if (bin_width <= 0) {
        bin_width <- (all_range[2] - all_range[1]) / 30
      }
      num_bins <- max(1, floor((all_range[2] - all_range[1]) / bin_width))
      common_breaks <- seq(all_range[1], all_range[2], length.out = num_bins + 1)
    } else {
      common_breaks <- seq(all_range[1], all_range[2], length.out = histogram.breaks + 1)
    }
  }
  
  # ------------------------------------------------------------------------
  # 4) Determine extended range for the x-axis
  # ------------------------------------------------------------------------
  data_range <- range(.data$X, na.rm = TRUE)
  range_width <- diff(data_range)
  if (range_width <= 0) {
    # All X are the same => expand artificially
    data_range <- data_range + c(-0.5, 0.5)
    range_width <- 1
  }
  if (xAxis.log) {
    # Must ensure positivity
    if (data_range[1] <= 0) {
      min_pos <- min(.data$X[.data$X > 0])
      data_range[1] <- min_pos
    }
    log_all_range <- log10(data_range)
    log_width <- diff(log_all_range)
    extended_log_range <- c(
      log_all_range[1] - xAxis.offset * log_width,
      log_all_range[2] + xAxis.offset * log_width
    )
    extended_range <- 10^extended_log_range
  } else {
    extended_range <- c(
      data_range[1] - xAxis.offset * range_width,
      data_range[2] + xAxis.offset * range_width
    )
  }
  
  # ------------------------------------------------------------------------
  # 5) Initialize highcharter object
  # ------------------------------------------------------------------------
  .p <- highchart() |>
    hc_add_theme(hc_theme_flat()) |>
    hc_chart(
      style = list(fontFamily = "Arial"),
      backgroundColor = "#FFFFFF",
      height = if (!is.null(plot.height)) paste0(plot.height, "px"),
      width  = if (!is.null(plot.width))  paste0(plot.width,  "px")
    )
  
  # Titles, if provided
  if (!is.null(plot.title)) {
    .p <- .p |> hc_title(text = plot.title)
  }
  if (!is.null(plot.subtitle)) {
    .p <- .p |> hc_subtitle(text = plot.subtitle)
  }
  
  # Axes
  .p <- .p |>
    hc_xAxis(
      title = list(text = if (xAxis.log) paste0(xAxis.legend, " (log scale)") else xAxis.legend),
      type  = if (xAxis.log) "logarithmic" else "linear",
      min   = extended_range[1],
      max   = extended_range[2],
      lineWidth = 1,
      lineColor = "black",
      gridLineWidth = 0.1,
      gridLineColor = "#E0E0E0",
      minorGridLineWidth = 0,
      tickLength = 5
    ) |>
    hc_yAxis(
      title = list(text = yAxis.legend),
      lineWidth = 1,
      lineColor = "black",
      gridLineWidth = 0.1,
      gridLineColor = "#E0E0E0",
      minorGridLineWidth = 0,
      tickLength = 5
    )
  
  # Legend
  .p <- .p |>
    hc_legend(
      enabled = legend.show,
      layout  = legend.layout,
      align   = legend.align,
      verticalAlign = legend.valign,
      itemStyle = list(
        fontSize   = "12px",
        fontWeight = "normal",
        fontFamily = "Arial"
      )
    ) |>
    hc_plotOptions(
      column = list(
        grouping = FALSE,
        pointPadding = 0,
        groupPadding = 0,
        borderWidth = 1,
        pointPlacement = "on"
      )
    )
  
  # ------------------------------------------------------------------------
  # 6) Add histograms (if requested)
  # ------------------------------------------------------------------------
  if ("histogram" %in% plot.type) {
    for (i in seq_along(.groups)) {
      group_data <- .data[ID == .groups[i]]
      xvals <- group_data$X
      
      if (xAxis.log) {
        xvals_pos <- xvals[xvals > 0]
        if (!length(xvals_pos)) next
        h <- hist(log10(xvals_pos), breaks = log10(common_breaks), plot = FALSE)
        hist_series <- lapply(seq_along(h$mids), function(j) {
          list(x = 10^h$mids[j], y = h$counts[j])
        })
      } else {
        h <- hist(xvals, breaks = common_breaks, plot = FALSE)
        hist_series <- lapply(seq_along(h$mids), function(j) {
          list(x = h$mids[j], y = h$counts[j])
        })
      }
      
      .p <- .p |>
        hc_add_series(
          name = paste(.groups[i], "Histogram"),
          data = hist_series,
          type = "column",
          color = get_color(i),
          borderWidth = 0.5,
          borderColor = "#FFFFFF",
          visible = TRUE   # show by default
        )
    }
  }
  
  # ------------------------------------------------------------------------
  # 7) Add densities (if requested)
  # ------------------------------------------------------------------------
  if ("density" %in% plot.type) {
    # For each group, compute density
    density_list <- .data[, {
      xvals <- X
      if (xAxis.log) {
        xvals_pos <- xvals[xvals > 0]
        if (!length(xvals_pos)) {
          list(x = numeric(0), y = numeric(0))
        } else {
          # density in log-space
          log_rng <- log10(extended_range)
          dens <- density(log10(xvals_pos), from = log_rng[1], to = log_rng[2], n = 512)
          # scale factor
          h  <- hist(log10(xvals_pos), breaks = log10(common_breaks), plot = FALSE)
          md <- max(dens$y, na.rm = TRUE)
          scale_factor <- if (md > 0) max(h$counts) / md else 1
          list(x = 10^dens$x, y = dens$y * scale_factor)
        }
      } else {
        dens <- density(xvals, from = extended_range[1], to = extended_range[2], n = 512)
        h    <- hist(xvals, breaks = common_breaks, plot = FALSE)
        md   <- max(dens$y, na.rm = TRUE)
        scale_factor <- if (md > 0) max(h$counts) / md else 1
        list(x = dens$x, y = dens$y * scale_factor)
      }
    }, by = ID]
    
    # Add each group's density as a separate series
    for (i in seq_along(.groups)) {
      grp  <- .groups[i]
      drow <- density_list[ID == grp]
      # drow$x and drow$y are list columns
      if (!length(drow$x[[1]])) next
      .p <- .p |>
        hc_add_series(
          name = paste(grp, "Density"),
          data = list_parse2(data.table(x = drow$x[[1]], y = drow$y[[1]])),
          type = "areaspline",
          color = get_color(i),
          fillOpacity = 0.4,
          lineWidth   = 3,
          marker      = list(enabled = FALSE),
          visible     = TRUE,  # show by default
          zIndex      = 3      # draw on top
        )
    }
  }
  
  # ------------------------------------------------------------------------
  # 8) Add quantile markers (if requested)
  # ------------------------------------------------------------------------
  if (isTRUE(plot.markers)) {
    for (i in seq_along(.groups)) {
      group_data <- .data[ID == .groups[i], X]
      if (!length(group_data)) next
      qs <- as.list(quantile(group_data, probs = c(0.05, 0.10, 0.16, 0.50, 0.84, 0.90, 0.95),
                             na.rm = TRUE))
      # Median
      .p <- .p |>
        hc_add_series(
          data = list(list(
            x = qs[[4]],  # median
            y = 0,
            name = "Median (50%)",
            value = round(qs[[4]], 4)
          )),
          name        = paste(.groups[i], "Median"),
          type        = "scatter",
          color       = "red",
          marker      = list(symbol = "circle", radius = 6, lineWidth = 1, lineColor = "black"),
          tooltip     = list(pointFormat = "{point.name}: {point.value}"),
          showInLegend = FALSE
        )
      
      # Other quantiles
      .p <- .p |>
        hc_add_series(
          data = list(
            list(x = qs[[1]], y = 0, name = "5%",  value = round(qs[[1]], 4)),
            list(x = qs[[2]], y = 0, name = "10%", value = round(qs[[2]], 4)),
            list(x = qs[[3]], y = 0, name = "16%", value = round(qs[[3]], 4)),
            list(x = qs[[5]], y = 0, name = "84%", value = round(qs[[5]], 4)),
            list(x = qs[[6]], y = 0, name = "90%", value = round(qs[[6]], 4)),
            list(x = qs[[7]], y = 0, name = "95%", value = round(qs[[7]], 4))
          ),
          name        = paste(.groups[i], "Quantiles"),
          type        = "scatter",
          color       = "black",
          marker      = list(symbol = "circle", radius = 4, lineWidth = 1, lineColor = "black"),
          tooltip     = list(pointFormat = "{point.name}: {point.value}"),
          showInLegend= FALSE
        )
    }
  }
  
  return(.p)
}
