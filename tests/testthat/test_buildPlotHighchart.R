# This test script demonstrates several scenarios for testing your buildPlot.highchart() function.
# It uses testthat conventions, though you can adapt it as needed in your workflow.
# Make sure your buildPlot.highchart function is sourced or loaded appropriately before running.

library(testthat)
library(highcharter)
library(data.table)

test_that("Line plots with different styles for two groups ID", {

  # Two groups, each with distinct line styles ("dashed" vs. "dotted")
  data.lines <- data.table::data.table(
    ID = rep(c("Group1", "Group2"), each = 5),
    X  = rep(1:5, 2),
    Y  = c(1, 3, 2, 4, 5, 2, 2.5, 3.5, 6, 7),
    style = c(rep("dashed", 5), rep("dotted", 5))
  )

  # No points data provided
  data.points <- NULL

  # Call the plotting function
  plot_obj <- buildPlot.highchart(
    data.lines  = data.lines,
    data.points = data.points,
    line.type   = "line",    # or "spline"
    xAxis.log   = FALSE,
    yAxis.log   = FALSE
  )

  # Basic checks
  expect_s3_class(plot_obj, "highchart")
})

test_that("Point plots with different styles for two groups ID", {

  # Two groups, each with distinct point styles ("triangle" vs. "square")
  data.points <- data.table::data.table(
    ID = rep(c("GroupA", "GroupB"), each = 4),
    X  = rep(1:4, 2),
    Y  = c(2, 5, 3, 7, 2, 6, 5, 8),
    style = c(rep("triangle", 4), rep("square", 4))
  )

  # No line data provided
  data.lines <- NULL

  plot_obj <- buildPlot.highchart(
    data.lines  = data.lines,
    data.points = data.points,
    line.type   = "line"
  )

  expect_s3_class(plot_obj, "highchart")
})

test_that("Points and lines (different data) for three group IDs, some overlap", {

  # Lines: three group IDs (1, 2, 3)
  data.lines <- data.table::data.table(
    ID = rep(c("G1", "G2", "G3"), each = 5),
    X  = rep(1:5, 3),
    Y  = c(seq(1, 5), seq(2, 6), seq(3, 7)),
    style = c(rep("solid", 5), rep("dotted", 5), rep("dashed", 5))
  )

  # Points: three group IDs (2, 3, 4) – notice overlap with G2/G3, plus a new G4
  data.points <- data.table::data.table(
    ID = rep(c("G2", "G3", "G4"), each = 4),
    X  = rep(1:4, 3),
    Y  = c(seq(10, 13), seq(7, 10), seq(5, 8)),
    style = c(rep("circle", 4), rep("square", 4), rep("triangle-down", 4))
  )

  plot_obj <- buildPlot.highchart(
    data.lines  = data.lines,
    data.points = data.points,
    line.type   = "spline"
  )

  expect_s3_class(plot_obj, "highchart")
})

test_that("Lines for 4 group IDs with fill for ID=2 and ID=4", {

  # Four groups: IDs "1", "2", "3", "4"
  # We'll fill lines for groups 2 and 4
  set.seed(123)
  data.lines <- data.table::data.table(
    ID   = rep(as.character(1:4), each = 6),
    X    = rep(1:6, 4),
    Y    = runif(24, min = 1, max = 10),  # some random values
    style= rep(c("solid", "dashed", "dotted", "dotdash"), each = 6),
    fill = rep(c(FALSE, TRUE, FALSE, TRUE), each = 6)  # fill for group 2 & 4
  )

  plot_obj <- buildPlot.highchart(
    data.lines  = data.lines,
    data.points = NULL,  # no scatter
    line.type   = "line",
    fill.polygon = FALSE   # deprecated, but data.lines$fill drives filling here
  )

  expect_s3_class(plot_obj, "highchart")
}) 