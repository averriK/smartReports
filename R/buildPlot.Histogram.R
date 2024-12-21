#' Build a histogram plot
#'
#' This function creates a histogram plot with density curves and statistical markers.
#'
#' @param .data A data.table with at least two columns: X (numeric) and ID (factor).
#' @param xTitle The title for the x-axis.
#' @param title The main title for the plot.
#' @param histogram.breaks The number of breaks for the histogram.
#' @param logscale Logical, whether to use a logarithmic scale for the x-axis.
#' @param offset The percentage of the range to extend the x-axis.
#' @import data.table
#' @importFrom stats quantile density
#' @importFrom graphics hist
#' @import highcharter
#' @return A highchart object representing the histogram plot.
#' @export buildPlot.Histogram



buildPlot.Histogram <- function(.data, xTitle = "", title = "", 
                                histogram.breaks = 100, logscale = FALSE, offset = 0.1) {
  
  # Get unique groups
  .groups <- unique(.data$ID)
  
  # Professional color palette
  .palette <- c("#4878D0", "#EE854A", "#6ACC64", "#D65F5F", "#956CB4", 
                "#82C6E2", "#D7B5A6", "#8B8B8B", "#B5DE2B", "#82C6E2")
  
  # Calculate common breaks for histograms (original range)
  if(logscale) {
    all_range <- log10(range(.data$X))
    common_breaks <- 10^seq(all_range[1], all_range[2], length.out = histogram.breaks + 1)
  } else {
    all_range <- range(.data$X)
    common_breaks <- seq(all_range[1], all_range[2], length.out = histogram.breaks + 1)
  }
  
  # Calculate extended range for consistent x-axis
  if(logscale) {
    # Work in log space to adjust the range proportionally
    log_all_range <- log10(all_range)
    range_width <- diff(log_all_range)
    extended_log_range <- c(
      log_all_range[1] - offset * range_width,
      log_all_range[2] + offset * range_width
    )
    # Convert back to original scale
    extended_range <- 10 ^ extended_log_range
  } else {
    range_width <- diff(all_range)
    extended_range <- c(
      all_range[1] - offset * range_width,
      all_range[2] + offset * range_width
    )
  }
  
  # Create base plot with consistent theme and extended x-axis range
  .p <- highchart() |>
    hc_add_theme(hc_theme_flat()) |>
    hc_chart(
      style = list(fontFamily = "Arial"),
      backgroundColor = "#FFFFFF"
    ) |>
    hc_title(text = title) |>
    hc_xAxis(
      title = list(text = if(logscale) paste(xTitle, "(log scale)") else xTitle),
      type = if(logscale) "logarithmic" else "linear",
      min = if(logscale) 10^extended_range[1] else extended_range[1],  # Set extended range
      max = if(logscale) 10^extended_range[2] else extended_range[2],  # Set extended range
      lineWidth = 1,
      lineColor = "black",
      gridLineWidth = 0.1,
      gridLineColor = "#E0E0E0",
      minorGridLineWidth = 0,
      tickLength = 5
    ) |>
    hc_yAxis(
      title = list(text = "Count"),
      lineWidth = 1,
      lineColor = "black",
      gridLineWidth = 0.1,
      gridLineColor = "#E0E0E0",
      minorGridLineWidth = 0,
      tickLength = 5
    ) |>
    hc_legend(
      layout = "vertical",
      align = "right",
      verticalAlign = "middle",
      itemStyle = list(
        fontSize = "12px",
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
  
  # First add all histograms
  for (i in seq_along(.groups)) {
    .group_data <- .data[ID == .groups[i]]
    
    # Calculate histogram with log breaks if needed
    if(logscale) {
      .hist_data <- hist(log10(.group_data$X), breaks = log10(common_breaks), plot = FALSE)
      .hist_series <- lapply(seq_along(.hist_data$mids), function(j) {
        list(x = 10^.hist_data$mids[j], y = .hist_data$counts[j])
      })
    } else {
      .hist_data <- hist(.group_data$X, breaks = common_breaks, plot = FALSE)
      .hist_series <- lapply(seq_along(.hist_data$mids), function(j) {
        list(x = .hist_data$mids[j], y = .hist_data$counts[j])
      })
    }
    
    # Add histogram series
    .p <- .p |>
      hc_add_series(
        name = paste(.groups[i], "Histogram"),
        data = .hist_series,
        type = "column",
        color = .palette[i],
        borderWidth = 0.5,
        borderColor = "#FFFFFF"
      )
  }
  
  # Then add all density plots as completely separate series
  density_list <- .data[, {
    # Get the full range for all data and extend it by 10% on each side
    full_range <- range(.data$X)
    range_width <- diff(full_range)
    extended_range <- c(full_range[1] - 0.1 * range_width,
                        full_range[2] + 0.1 * range_width)
    
    if(logscale) {
      # Filter positive X values
      X_pos <- X[X > 0]
      
      # Adjust extended_range for densities
      min_positive <- min(X_pos, na.rm = TRUE)
      if (is.infinite(min_positive)) {
        stop("Data contains no positive values, cannot compute density")
      }
      full_range <- range(X_pos)
      full_range[1] <- max(full_range[1], min_positive / 10)
      
      log_full_range <- log10(full_range)
      range_width <- diff(log_full_range)
      extended_log_range <- c(
        log_full_range[1] - offset * range_width,
        log_full_range[2] + offset * range_width
      )
      extended_range <- 10 ^ extended_log_range
      
      # Ensure extended_range[1] is positive
      if(extended_range[1] <= 0) {
        extended_range[1] <- full_range[1] / 10
        extended_log_range[1] <- log10(extended_range[1])
      }
      
      dens <- density(log10(X_pos), from = extended_log_range[1], 
                      to = extended_log_range[2], n = 512)
      
      # Calculate histogram with same breaks as main plot for proper scaling
      .hist <- hist(log10(X_pos), breaks = log10(common_breaks), plot = FALSE)
      scale_factor <- max(.hist$counts) / max(dens$y, na.rm = TRUE)
      list(x = 10^dens$x, y = dens$y * scale_factor)
    } else {
      # Original code for linear scale
      full_range <- range(X)
      range_width <- diff(full_range)
      extended_range <- c(full_range[1] - offset * range_width,
                          full_range[2] + offset * range_width)
      dens <- density(X, from = extended_range[1], 
                      to = extended_range[2], n = 512)
      .hist <- hist(X, breaks = common_breaks, plot = FALSE)
      scale_factor <- max(.hist$counts) / max(dens$y, na.rm = TRUE)
      list(x = dens$x, y = dens$y * scale_factor)
    }
  }, by = ID]
  
  # Define density colors (slightly different than histogram colors)
  .density_palette <- c("#7AA6FF", "#FFB07A", "#90E088", "#FF8A8A", "#BE99E3", 
                        "#A6E4FF", "#FFD7C6", "#B3B3B3", "#D9FF5C", "#A6E4FF")
  
  for (i in seq_along(.groups)) {
    group_data <- density_list[ID == .groups[i]]
    .p <- .p |> 
      hc_add_series(
        data = list_parse2(data.table(x = group_data$x, y = group_data$y)),
        name = paste(.groups[i], "Density"),
        type = "areaspline",
        color = .density_palette[i],
        fillOpacity = 0.75,    # Increased opacity from 0.3 to 0.5
        lineWidth = 4,
        marker = list(enabled = FALSE),
        visible = FALSE,
        zIndex = 1            # Try to control layering
      )
  }
  
  # Add statistical markers for each group
  for (i in seq_along(.groups)) {
    group_data <- .data[ID == .groups[i]]
    stats <- as.list(quantile(group_data$X, probs = c(0.05, 0.10, 0.16, 0.50, 0.84, 0.90, 0.95)))
    
    # Add median as a special point
    .p <- .p |>
      hc_add_series(
        data = list(list(
          x = stats[[4]], 
          y = 0,
          name = "Median (50%)",
          value = round(stats[[4]], 4)
        )),
        name = paste(.groups[i], "Median"),
        type = "scatter",
        color = "red",
        marker = list(
          symbol = "circle",
          radius = 6,
          lineWidth = 1,
          lineColor = "black"
        ),
        tooltip = list(
          pointFormat = "{point.name}: {point.value}"
        ),
        showInLegend = FALSE
      )
    
    # Add other quantiles
    quantile_names <- c("5%", "10%", "16%", "84%", "90%", "95%")
    .p <- .p |>
      hc_add_series(
        data = list(
          list(x = stats[[1]], y = 0, name = "5th percentile", value = round(stats[[1]], 4)),
          list(x = stats[[2]], y = 0, name = "10th percentile", value = round(stats[[2]], 4)),
          list(x = stats[[3]], y = 0, name = "16th percentile", value = round(stats[[3]], 4)),
          list(x = stats[[5]], y = 0, name = "84th percentile", value = round(stats[[5]], 4)),
          list(x = stats[[6]], y = 0, name = "90th percentile", value = round(stats[[6]], 4)),
          list(x = stats[[7]], y = 0, name = "95th percentile", value = round(stats[[7]], 4))
        ),
        name = paste(.groups[i], "Quantiles"),
        type = "scatter",
        color = "black",
        marker = list(
          symbol = "circle",
          radius = 4,
          lineWidth = 1,
          lineColor = "black"
        ),
        tooltip = list(
          pointFormat = "{point.name}: {point.value}"
        ),
        showInLegend = FALSE
      )
  }
  
  return(.p)
}

# nolint end
