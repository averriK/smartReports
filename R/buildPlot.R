#' @title Build a highchart plot
#' @description Build a highchart plot using separate data sources for lines and points.
#'
#' Supports the plotting of line and scatter series. Includes optional arearange shading between
#' two line groups if indicated in `data.lines$fill`, and interpolation is handled via the `approx` function
#' (which can be customized using the `interpolation.method` argument). Fallback behaviors:
#' if an invalid `line.type` is provided, `"line"` is used; if an invalid or missing line style is found,
#' `"solid"` is used; if an invalid `color.palette` is provided, a default Highcharts palette is chosen.
#'
#' @param library DEPRECATED. A placeholder parameter that triggers a warning if used.
#' @param plot.type DEPRECATED. A placeholder parameter that triggers a warning if used.
#' @param data.lines A data.table containing columns "ID", "X", "Y", optional "style", optional "fill".
#'                   If NULL, no lines will be plotted.
#' @param data.points A data.table containing columns "ID", "X", "Y", optional "style".
#'                    If NULL, no scatter points will be plotted.
#' @param line.type A string indicating the line series type. Options might include `"line"` or `"spline"`.
#'                  If invalid, `"line"` is used.
#' @param plot.title A string for the plot title
#' @param plot.subtitle A string for the plot subtitle
#' @param plot.height A numeric for the plot height
#' @param plot.width A numeric for the plot width
#' @param xAxis.legend A string for the x-axis legend
#' @param yAxis.legend A string for the y-axis legend
#' @param group.legend A string for the legend title
#' @param color.palette A string for the color palette (must exist in `grDevices::hcl.pals()`)
#' @param line.style A string specifying the default line style if `data.lines$style` is missing/invalid.
#'                   Valid examples include `"solid"`, `"dashed"`, `"dotted"`, etc.
#' @param point.style A string specifying the default point marker style if `data.points$style` is missing/invalid.
#'                    Valid examples: `"circle"`, `"square"`, `"diamond"`, etc.
#' @param line.size A numeric for the line width
#' @param point.size A numeric for the point size
#' @param xAxis.log A logical for the x-axis log scale
#' @param yAxis.log A logical for the y-axis log scale
#' @param xAxis.reverse A logical for the x-axis reverse
#' @param yAxis.reverse A logical for the y-axis reverse
#' @param xAxis.max A numeric for the x-axis max
#' @param yAxis.max A numeric for the y-axis max
#' @param xAxis.min A numeric for the x-axis min
#' @param yAxis.min A numeric for the y-axis min
#' @param xAxis.label A logical for the x-axis label
#' @param yAxis.label A logical for the y-axis label
#' @param legend.layout A string for the legend layout
#' @param legend.align A string for the legend horizontal alignment (e.g. `"center"`, `"left"`, `"right"`)
#' @param legend.valign A string for the legend vertical alignment (e.g. `"top"`, `"middle"`, `"bottom"`)
#' @param legend.show A logical for the legend show/hide
#' @param plot.save A logical for the plot save
#' @param plot.theme A highchart theme object
#' @param xAxis.legend.fontsize A string for the x-axis legend fontsize
#' @param yAxis.legend.fontsize A string for the y-axis legend fontsize
#' @param group.legend.fontsize A string for the legend items' fontsize
#' @param plot.title.fontsize A string for the plot title fontsize
#' @param plot.subtitle.fontsize A string for the plot subtitle fontsize
#' @param print.max.abs A logical for printing max absolute Y labels as annotations (only for lines)
#' @param point.dataLabels A logical for whether data labels appear for points
#' @param plot.filename A string for the plot filename, if saving
#' @param interpolation.method A string specifying the interpolation method used by `approx()`.
#'                             Defaults to `"linear"`. Other valid values might include `"constant"`, `"spline"`.
#'
#' @return A highchart object if either `data.lines` or `data.points` is provided.
#'         Returns NULL if both are NULL, with a soft warning.
#' @importFrom grDevices hcl.pals hcl.colors
#' @importFrom stats setNames approx
#' @import highcharter
#' @importFrom htmlwidgets saveWidget
#' @export buildPlot
#'
buildPlot <- function(
    library = NULL, # DEPRECATED: triggers a warning if explicitly used
    plot.type = NULL, # DEPRECATED: triggers a warning if explicitly used
    data.lines = NULL,
    data.points = NULL,
    line.type = "line", # e.g. "line", "spline", or fallback
    plot.title = NULL,
    plot.subtitle = NULL,
    plot.height = NULL,
    plot.width = NULL,
    xAxis.legend = "X",
    yAxis.legend = "Y",
    group.legend = "ID",
    color.palette = "Dark 3",
    line.style = "solid", # default fallback for lines if style is missing or invalid
    point.style = "circle", # default fallback for points if style is missing or invalid
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
    legend.align = "right",
    legend.valign = "top",
    legend.show = TRUE,
    plot.save = FALSE,
    plot.theme = NULL,
    xAxis.legend.fontsize = "14px",
    yAxis.legend.fontsize = "14px",
    group.legend.fontsize = "12px",
    plot.title.fontsize = "24px",
    plot.subtitle.fontsize = "18px",
    print.max.abs = FALSE,
    point.dataLabels = FALSE,
    plot.filename = NULL,
    interpolation.method = "linear") {
    ## 1. Deprecation warnings for library, plot.type
    #    only trigger if the user explicitly set them (i.e. not missing)
    if (!missing(library)) {
        warning("'library' is deprecated and will be ignored.")
    }
    if (!missing(plot.type)) {
        warning("'plot.type' is deprecated and will be ignored.")
    }

    ## 2. Soft check if both data.lines and data.points are NULL
    if (is.null(data.lines) && is.null(data.points)) {
        warning("No data provided for lines or points. Returning NULL.")
        return(NULL)
    }

    ## 3. Validate numeric inputs (warn & set default, instead of stop)
    if (!is.numeric(line.size) || line.size <= 0) {
        warning(
            sprintf("line.size should be a positive numeric. Using default (1) instead of '%s'.", line.size)
        )
        line.size <- 1
    }
    if (!is.numeric(point.size) || point.size <= 0) {
        warning(
            sprintf("point.size should be a positive numeric. Using default (3) instead of '%s'.", point.size)
        )
        point.size <- 3
    }

    ## 4. Validate color palette
    if (!color.palette %in% grDevices::hcl.pals()) {
        warning("Invalid color palette. Using default.")
        # pick some default from the list
        color.palette <- grDevices::hcl.pals()[4]
    }

    ## 5. Name mappings for line dash styles (all-lowercase keys)
    #    The final Highcharts dashStyle strings are typically capitalized, but
    #    we keep the map keys in lowercase for consistent matching.
    LINE.STYLE <- list(
        "solid"          = "Solid",
        "dashed"         = "Dash",
        "dotted"         = "Dot",
        "dotdash"        = "DashDot",
        "longdash"       = "LongDash",
        "twodash"        = "Dash",
        "shortdash"      = "ShortDash",
        "shortdot"       = "ShortDot",
        "shortdashdot"   = "ShortDashDot",
        "longdashdotdot" = "LongDashDotDot"
    )

    ## 6. Name mappings for point marker styles (lowercase keys)
    POINT.STYLE <- list(
        "circle"        = "circle",
        "square"        = "square",
        "diamond"       = "diamond",
        "triangle"      = "triangle",
        "triangle-down" = "triangle-down"
    )

    ## 7. Validate line.type
    line.type <- tolower(line.type)
    # Potentially allow more than "line"/"spline" if you like:
    VALID.LINE.TYPES <- c("line", "spline")
    if (!line.type %in% VALID.LINE.TYPES) {
        warning(sprintf("Provided line.type='%s' is invalid. Using 'line' by default.", line.type))
        line.type <- "line"
    }

    ## 8. Validate data inputs
    validateData <- function(data, data_name) {
        if (!is.null(data)) {
            if (!all(c("ID", "X", "Y") %in% colnames(data))) {
                stop(sprintf("%s must contain columns named ID, X, and Y if not NULL.", data_name))
            }
            if (!is.numeric(data$Y)) {
                stop(sprintf("Error in buildPlot(): %s$Y must be numeric.", data_name))
            }
        }
    }
    validateData(data.lines, "data.lines")
    validateData(data.points, "data.points")

    ## 9. Validate interpolation.method
    valid.methods <- c("linear", "constant", "spline")
    if (!interpolation.method %in% valid.methods) {
        warning(sprintf(
            "Provided interpolation.method='%s' is invalid. Using 'linear' by default.",
            interpolation.method
        ))
        interpolation.method <- "linear"
    }

    ## 10. Build color mapping (collect all IDs for consistent color usage)
    ALL.IDS <- unique(c(
        if (!is.null(data.lines)) data.lines$ID,
        if (!is.null(data.points)) data.points$ID
    ))
    ID.COLOR.MAP <- stats::setNames(
        grDevices::hcl.colors(
            n = length(ALL.IDS),
            palette = color.palette
        ),
        as.character(ALL.IDS)
    )

    ## 11. Initialize plot object
    plot.object <- highchart() |>
        hc_xAxis(
            labels = list(enabled = xAxis.label),
            title = list(
                text = xAxis.legend,
                style = list(fontSize = xAxis.legend.fontsize)
            ),
            type = if (xAxis.log) "logarithmic" else "linear",
            reversed = xAxis.reverse,
            max = if (!is.na(xAxis.max)) xAxis.max else NULL,
            min = if (!is.na(xAxis.min)) xAxis.min else NULL
        ) |>
        hc_yAxis(
            labels = list(enabled = yAxis.label),
            title = list(
                text = yAxis.legend,
                style = list(fontSize = yAxis.legend.fontsize)
            ),
            type = if (yAxis.log) "logarithmic" else "linear",
            reversed = yAxis.reverse,
            max = if (!is.na(yAxis.max)) yAxis.max else NULL,
            min = if (!is.na(yAxis.min)) yAxis.min else NULL
        )

    ## 12. Theme handling
    if (!is.null(plot.theme)) {
        plot.object <- plot.object |> hc_add_theme(plot.theme)
    } else {
        plot.object <- plot.object |> hc_add_theme(hc_theme_flat())
    }

    ## 13. Titles
    if (!is.null(plot.title)) {
        plot.object <- plot.object |>
            hc_title(
                text = plot.title,
                style = list(fontSize = plot.title.fontsize)
            )
    }
    if (!is.null(plot.subtitle)) {
        plot.object <- plot.object |>
            hc_subtitle(
                text = plot.subtitle,
                style = list(fontSize = plot.subtitle.fontsize)
            )
    }

    ## 14. Plot size
    if (!is.null(plot.height) || !is.null(plot.width)) {
        plot.object <- plot.object |>
            hc_size(height = plot.height, width = plot.width)
    }

    ## 15. Legend (add legend title using group.legend)
    plot.object <- plot.object |>
        hc_legend(
            enabled = legend.show,
            align = legend.align,
            verticalAlign = legend.valign,
            layout = legend.layout,
            title = list(
                text = group.legend,
                style = list(fontWeight = "bold") # optional
            ),
            itemStyle = list(fontSize = group.legend.fontsize)
        ) |>
        hc_chart(style = list(fontFamily = "Helvetica"))

    ## ============ LINES =============
    if (!is.null(data.lines)) {
        unique_lines <- unique(data.lines$ID)

        for (gid in unique_lines) {
            sub_data <- data.lines[ID == gid]
            # Determine line style from data or fallback
            if ("style" %in% names(sub_data)) {
                style_val <- tolower(as.character(sub_data$style[1]))
            } else {
                style_val <- tolower(line.style) # fallback
            }

            dash_style <- LINE.STYLE[[style_val]]
            if (is.null(dash_style)) {
                warning(sprintf(
                    "Invalid line style '%s' for ID='%s'. Using fallback 'solid'.",
                    style_val, gid
                ))
                dash_style <- LINE.STYLE[["solid"]] # final fallback
            }

            # Build line series
            plot.object <- plot.object |>
                hc_add_series(
                    data = sub_data,
                    type = line.type,
                    hcaes(x = X, y = Y),
                    name = as.character(gid),
                    color = ID.COLOR.MAP[as.character(gid)],
                    dashStyle = dash_style,
                    lineWidth = line.size,
                    marker = list(enabled = FALSE)
                )
        }

        ## ============ AREA-FILL =============
        if ("fill" %in% names(data.lines)) {
            ID.FILL <- unique(data.lines[fill == TRUE, ID])

            if (length(ID.FILL) > 2) {
                warning("More than two IDs marked for filling. Only the first two will be used.")
                ID.FILL <- ID.FILL[1:2]
            }
            if (length(ID.FILL) == 2) {
                # Fill area between exactly two sets
                gid1 <- ID.FILL[1]
                gid2 <- ID.FILL[2]
                xvals <- sort(unique(c(
                    data.lines[ID == gid1]$X,
                    data.lines[ID == gid2]$X
                )))
                curve1 <- approx(
                    data.lines[ID == gid1]$X,
                    data.lines[ID == gid1]$Y,
                    xout = xvals,
                    method = interpolation.method
                )
                curve2 <- approx(
                    data.lines[ID == gid2]$X,
                    data.lines[ID == gid2]$Y,
                    xout = xvals,
                    method = interpolation.method
                )
                fill_data <- data.frame(
                    X = xvals,
                    LOW = pmin(curve1$y, curve2$y),
                    HIGH = pmax(curve1$y, curve2$y)
                )

                plot.object <- plot.object |>
                    hc_add_series(
                        data = fill_data,
                        type = "arearange",
                        hcaes(x = X, low = LOW, high = HIGH),
                        name = paste("Area between", gid1, "and", gid2),
                        color = ID.COLOR.MAP[as.character(gid1)],
                        fillOpacity = 0.3 # Adjust transparency as needed
                    )
            }
        }
    }

    ## ============ POINTS =============
    if (!is.null(data.points)) {
        unique_points <- unique(data.points$ID)

        for (gid in unique_points) {
            sub_data <- data.points[ID == gid]
            # Determine point style from data or fallback
            if ("style" %in% names(sub_data)) {
                style_val <- tolower(as.character(sub_data$style[1]))
            } else {
                style_val <- tolower(point.style)
            }

            symbol_style <- POINT.STYLE[[style_val]]
            if (is.null(symbol_style)) {
                warning(sprintf(
                    "Invalid point style '%s' for ID='%s'. Using fallback 'circle'.",
                    style_val, gid
                ))
                symbol_style <- POINT.STYLE[["circle"]]
            }

            # Build scatter series
            plot.object <- plot.object |>
                hc_add_series(
                    data = sub_data,
                    type = "scatter",
                    hcaes(x = X, y = Y),
                    name = as.character(gid),
                    color = ID.COLOR.MAP[as.character(gid)],
                    marker = list(
                        symbol = symbol_style,
                        radius = point.size
                    )
                )
        }
    }

    ## 16. Tooltip
    plot.object <- plot.object |>
        hc_tooltip(
            sort = FALSE,
            split = FALSE,
            crosshairs = TRUE,
            headerFormat = "",
            pointFormat = sprintf(
                "<b>{point.series.name}</b><br>%s: {point.x}<br>%s: {point.y}",
                xAxis.legend,
                yAxis.legend
            )
        ) |>
        hc_plotOptions(
            series = list(
                dataLabels = list(enabled = point.dataLabels)
            )
        )

    ## 17. Print max absolute value (applied to data.lines only)
    if (print.max.abs && !is.null(data.lines)) {
        # data.lines$Y is already validated numeric above
        data_abs <- data.lines[, .SD[which.max(abs(Y))], by = ID]
        plot.object <- plot.object |>
            hc_annotations(
                list(
                    labels = lapply(seq_len(nrow(data_abs)), function(i) {
                        list(
                            point = list(
                                xAxis = 0, yAxis = 0,
                                x = data_abs$X[i], y = data_abs$Y[i]
                            ),
                            text = paste0("Max Abs: ", round(data_abs$Y[i], 2))
                        )
                    })
                )
            )
    }

    ## 18. Save if requested
    if (plot.save) {
        if (is.null(plot.filename)) {
            plot.filename <- "plot.html"
        }
        saveWidget(widget = plot.object, file = plot.filename)
    }

    return(plot.object)
}
