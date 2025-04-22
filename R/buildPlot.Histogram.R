#' Build a histogram (and/or density) plot
#'
#' Creates a histogram and/or density plot, with optional log-scale, quantile markers,
#' and Freedman–Diaconis bin calculation. This version ensures density scaling is computed
#' even if only density is plotted (so the density curve appears).
#'
#' @param .data A data.table with columns "X" (numeric) and "ID" (factor or character).
#' @param plot.type Character vector: \code{"histogram"}, \code{"density"}, or both, e.g. \code{c("histogram","density")}.
#' @param histogram.breaks Either NULL (automatic Freedman–Diaconis) or numeric (number of bins).
#' @param xAxis.log Logical; if TRUE, use log-scale on the X data. Default FALSE.
#' @param xAxis.offset Numeric; fraction to expand the x-axis range on each side. Default 0.1.
#' @param xAxis.legend Character label for the x-axis (display only). Default "X axis".
#' @param yAxis.legend Character label for the y-axis. Default "Count".
#' @param plot.title  Character; main title. Default NULL.
#' @param plot.subtitle Character; optional subtitle. Default NULL.
#' @param plot.markers Logical; if TRUE, add quantile markers. Default FALSE.
#' @param color.palette Either a character vector of colors or a single string fallback. Default "Dark3".
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
  # 0) Validate .data
  # ------------------------------------------------------------------------
  plot.type <- match.arg(plot.type, choices = c("histogram", "density"), several.ok = TRUE)
  if (!inherits(.data, "data.table")) {
    stop("`.data` must be a data.table.")
  }
  if (!all(c("X","ID") %in% names(.data))) {
    stop("`.data` must have columns named 'X' (numeric) and 'ID' (factor/character).")
  }
  if (!is.numeric(.data$X)) {
    stop("Column 'X' must be numeric.")
  }
  if (!is.factor(.data$ID)) {
    # Convert character ID to factor if needed
    if (is.character(.data$ID)) {
      .data[, ID := factor(ID)]
    } else {
      stop("Column 'ID' must be factor or character.")
    }
  }
  
  .groups <- unique(.data$ID)
  
  # ------------------------------------------------------------------------
  # 1) Manage color palette
  # ------------------------------------------------------------------------
  process_palette <- function(pal_input, n_needed) {
    # If user provided multiple colors, use them
    if (length(pal_input) > 1) {
      return(pal_input)
    }
    # Otherwise fallback to a basic set
    fallback <- c("#4878D0", "#EE854A", "#6ACC64", "#D65F5F", "#956CB4", 
                  "#82C6E2", "#D7B5A6", "#8B8B8B", "#B5DE2B", "#82C6E2")
    return(fallback)
  }
  pal <- process_palette(color.palette, length(.groups))
  get_color <- function(i) pal[((i - 1) %% length(pal)) + 1]
  
  # ------------------------------------------------------------------------
  # 2) Compute Freedman–Diaconis (or user-based) breaks for *reference*
  #    Always do this, even if only density is requested, so we can scale.
  # ------------------------------------------------------------------------
  if (xAxis.log) {
    # Filter to positive values
    x_pos <- .data$X[.data$X > 0]
    if (!length(x_pos)) {
      stop("No positive values for log-scale.")
    }
    if (is.null(histogram.breaks)) {
      logX <- log10(x_pos)
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
      rng <- range(x_pos, na.rm = TRUE)
      common_breaks <- 10^seq(log10(rng[1]), log10(rng[2]), length.out = histogram.breaks + 1)
    }
  } else {
    rng <- range(.data$X, na.rm = TRUE)
    if (is.null(histogram.breaks)) {
      x_all <- na.omit(.data$X)
      iqr_x <- IQR(x_all, na.rm = TRUE)
      n <- length(x_all)
      bin_width <- 2 * iqr_x / (n^(1/3))
      if (bin_width <= 0) {
        bin_width <- (rng[2] - rng[1]) / 30
      }
      num_bins <- max(1, floor((rng[2] - rng[1]) / bin_width))
      common_breaks <- seq(rng[1], rng[2], length.out = num_bins + 1)
    } else {
      common_breaks <- seq(rng[1], rng[2], length.out = histogram.breaks + 1)
    }
  }
  
  # ------------------------------------------------------------------------
  # 3) Extended x-range
  # ------------------------------------------------------------------------
  data_range <- range(.data$X, na.rm = TRUE)
  range_width <- diff(data_range)
  if (range_width <= 0) {
    data_range <- data_range + c(-0.5, 0.5)
    range_width <- 1
  }
  if (xAxis.log) {
    if (data_range[1] <= 0) {
      min_pos <- min(.data$X[.data$X > 0])
      data_range[1] <- min_pos
    }
    log_rg <- log10(data_range)
    lw <- diff(log_rg)
    ext_log_range <- c(log_rg[1] - xAxis.offset * lw,
                       log_rg[2] + xAxis.offset * lw)
    extended_range <- 10^ext_log_range
  } else {
    extended_range <- c(
      data_range[1] - xAxis.offset * range_width,
      data_range[2] + xAxis.offset * range_width
    )
  }
  
  # ------------------------------------------------------------------------
  # 4) Initialize Highchart
  # ------------------------------------------------------------------------
  .p <- highchart() |>
    hc_add_theme(hc_theme_flat()) |>
    hc_chart(
      style = list(fontFamily = "Arial"),
      backgroundColor = "#FFFFFF",
      height = if (!is.null(plot.height)) paste0(plot.height, "px"),
      width  = if (!is.null(plot.width))  paste0(plot.width,  "px")
    )
  
  if (!is.null(plot.title)) {
    .p <- .p |> hc_title(text = plot.title)
  }
  if (!is.null(plot.subtitle)) {
    .p <- .p |> hc_subtitle(text = plot.subtitle)
  }
  
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
    ) |>
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
  # 5) Compute reference histograms & densities for scaling
  #    Even if user wants only density, we do the hist for scale factor.
  #    Then we add the relevant series if requested.
  # ------------------------------------------------------------------------
  for (i in seq_along(.groups)) {
    grp <- .groups[i]
    subdata <- .data[ID == grp]
    xvals <- subdata$X
    
    # 5a) reference histogram for scaling the density
    if (xAxis.log) {
      xvals_pos <- xvals[xvals > 0]
      if (!length(xvals_pos)) {
        # skip this group entirely if no positive data
        next
      }
      h <- hist(log10(xvals_pos), breaks = log10(common_breaks), plot = FALSE)
      # We'll create an unscaled histogram series for actual plotting (if needed)
      hist_series <- lapply(seq_along(h$mids), function(j) {
        list(x = 10^h$mids[j], y = h$counts[j])
      })
    } else {
      h <- hist(xvals, breaks = common_breaks, plot = FALSE)
      hist_series <- lapply(seq_along(h$mids), function(j) {
        list(x = h$mids[j], y = h$counts[j])
      })
    }
    max_count <- if (length(h$counts)) max(h$counts) else 0
    
    # 5b) scaled density
    # We always compute it here, but only add it if "density" %in% plot.type
    if (xAxis.log) {
      # must filter positive
      if (!length(xvals_pos)) {
        next
      }
      log_rng <- log10(extended_range)
      dens <- density(log10(xvals_pos), from = log_rng[1], to = log_rng[2], n = 512)
      max_dens <- max(dens$y, na.rm = TRUE)
      scale_factor <- if (max_dens > 0) max_count / max_dens else 1
      density_series_data <- data.table(x = 10^dens$x, y = dens$y * scale_factor)
    } else {
      dens <- density(xvals, from = extended_range[1], to = extended_range[2], n = 512)
      max_dens <- max(dens$y, na.rm = TRUE)
      scale_factor <- if (max_dens > 0) max_count / max_dens else 1
      density_series_data <- data.table(x = dens$x, y = dens$y * scale_factor)
    }
    
    # ----------------------------------------------------------------------
    # 6) Add the histogram series if needed
    # ----------------------------------------------------------------------
    if ("histogram" %in% plot.type && length(h$counts) > 0) {
      .p <- .p |>
        hc_add_series(
          name = paste(grp, "Histogram"),
          data = hist_series,
          type = "column",
          color = get_color(i),
          borderWidth = 0.5,
          borderColor = "#FFFFFF",
          visible = TRUE
        )
    }
    
    # ----------------------------------------------------------------------
    # 7) Add the density series if needed
    # ----------------------------------------------------------------------
    if ("density" %in% plot.type && nrow(density_series_data) > 0) {
      .p <- .p |>
        hc_add_series(
          name = paste(grp, "Density"),
          data = list_parse2(density_series_data),
          type = "areaspline",
          color = get_color(i),
          fillOpacity = 0.4,
          lineWidth = 3,
          marker = list(enabled = FALSE),
          visible = TRUE, 
          zIndex = 3
        )
    }
    
    # ----------------------------------------------------------------------
    # 8) Add quantile markers if requested
    # ----------------------------------------------------------------------
    if (isTRUE(plot.markers) && length(xvals) > 0) {
      qs <- as.list(quantile(xvals, probs = c(0.05, 0.10, 0.16, 0.50, 0.84, 0.90, 0.95),
                             na.rm = TRUE))
      # median
      .p <- .p |>
        hc_add_series(
          data = list(list(
            x = qs[[4]],
            y = 0,
            name = "Median (50%)",
            value = round(qs[[4]], 4)
          )),
          name = paste(grp, "Median"),
          type = "scatter",
          color = "red",
          marker = list(symbol = "circle", radius = 6, lineWidth = 1, lineColor = "black"),
          tooltip = list(pointFormat = "{point.name}: {point.value}"),
          showInLegend = FALSE
        )
      # others
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
          name = paste(grp, "Quantiles"),
          type = "scatter",
          color = "black",
          marker = list(symbol = "circle", radius = 4, lineWidth = 1, lineColor = "black"),
          tooltip = list(pointFormat = "{point.name}: {point.value}"),
          showInLegend = FALSE
        )
    }
  }
  
  return(.p)
}
