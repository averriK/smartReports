# smartReports

## Overview

The `smartReports` package is designed to simplify and streamline the creation of plots, tables, and reports in R. It comprises three main functions:

- **`buildPlot`**: Facilitates the creation of various types of plots using `ggplot2`, `highcharter`, or `plotly`.
- **`buildTable`**: Renders tables using `flextable`, `gt`, or `kableExtra`, offering extensive formatting options for different output formats.
- **`buildReport`**: Generates comprehensive reports in multiple formats such as HTML, PDF, or Word using `quarto`, integrating plots and tables created with `buildPlot` and `buildTable`.

The `smartReports` package provides a unified interface for generating visualizations, tables, and reports with consistent styling and customization options.

---

## Installation

To install the development version of `smartReports` from GitHub, use the following commands:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install smartReports from GitHub
devtools::install_github("averriK/smartReports")
```

---

## Functions

### buildPlot

Creates customized plots using `ggplot2`, `highcharter`, or `plotly` based on the provided parameters.

#### Usage:

```r
buildPlot(
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
  plot.type = "line",
  line.style = NULL,
  point.style = 16,
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
  plot.save = TRUE,
  plot.theme = NULL,
  xAxis.legend.fontsize = "14px",
  yAxis.legend.fontsize = "14px",
  group.legend.fontsize = "12px",
  plot.title.fontsize = "24px",
  plot.subtitle.fontsize = "18px",
  print.max.abs = FALSE,
  fill.polygon = FALSE,
  fill.group = "",
  point.marker = FALSE,
  point.dataLabels = FALSE
)
```

#### Arguments:

- **`data`**: A data frame containing the variables to plot.
- **`library`**: The plotting library to use. Options are `"highcharter"`, `"ggplot2"`, or `"plotly"`.
- **`plot.object`**: An existing plot object to which layers can be added.
- **`plot.title`**: The title of the plot.
- **`plot.subtitle`**: The subtitle of the plot.
- **`plot.height`**: The height of the plot (numeric).
- **`plot.width`**: The width of the plot (numeric).
- **`xAxis.legend`**: Label for the X-axis.
- **`yAxis.legend`**: Label for the Y-axis.
- **`group.legend`**: Label for the group legend.
- **`color.palette`**: Color palette to use for the plot.
- **`plot.type`**: Type of plot to create. Options are `"line"`, `"spline"`, `"point"`, `"column"`, `"bar"`.
- **`line.style`**: Style of the line. Options depend on the plotting library.
- **`point.style`**: Style of the points. Options depend on the plotting library.
- **`line.size`**: Size of the line.
- **`point.size`**: Size of the points.
- **`xAxis.log`**: Logical. Should the X-axis be logarithmic?
- **`yAxis.log`**: Logical. Should the Y-axis be logarithmic?
- **`xAxis.reverse`**: Logical. Should the X-axis be reversed?
- **`yAxis.reverse`**: Logical. Should the Y-axis be reversed?
- **`xAxis.max`**: Maximum value for the X-axis.
- **`yAxis.max`**: Maximum value for the Y-axis.
- **`xAxis.min`**: Minimum value for the X-axis.
- **`yAxis.min`**: Minimum value for the Y-axis.
- **`xAxis.label`**: Logical. Should the X-axis labels be displayed?
- **`yAxis.label`**: Logical. Should the Y-axis labels be displayed?
- **`legend.layout`**: Layout of the legend. Options are `"horizontal"` or `"vertical"`.
- **`legend.align`**: Alignment of the legend. Options are `"left"`, `"center"`, or `"right"`.
- **`legend.valign`**: Vertical alignment of the legend. Options are `"top"`, `"middle"`, or `"bottom"`.
- **`legend.show`**: Logical. Should the legend be displayed?
- **`plot.save`**: Logical. Should the plot be saved?
- **`plot.theme`**: Theme to apply to the plot.
- **`xAxis.legend.fontsize`**: Font size for the X-axis legend.
- **`yAxis.legend.fontsize`**: Font size for the Y-axis legend.
- **`group.legend.fontsize`**: Font size for the group legend.
- **`plot.title.fontsize`**: Font size for the plot title.
- **`plot.subtitle.fontsize`**: Font size for the plot subtitle.
- **`print.max.abs`**: Logical. If `TRUE`, prints the maximum absolute values.
- **`fill.polygon`**: Logical. If `TRUE`, fills the area under the plot.
- **`fill.group`**: Variable name to fill different groups.
- **`point.marker`**: Logical. Should point markers be displayed?
- **`point.dataLabels`**: Logical. Should data labels be displayed on points.

#### Example:

```r
library(smartReports)

# Prepare data
DT <- data.frame(ID = iris$Species, X = iris$Sepal.Length, Y = iris$Sepal.Width)

# Create a line plot using highcharter
buildPlot(
  data = DT,
  library = "highcharter",
  plot.type = "line",
  plot.title = "Sepal Dimensions",
  xAxis.legend = "Sepal Length",
  yAxis.legend = "Sepal Width",
  color.palette = "Set1",
  legend.align = "right",
  legend.valign = "top"
)

# Create a scatter plot using ggplot2
buildPlot(
  data = DT,
  library = "ggplot2",
  plot.type = "point",
  plot.title = "Sepal Dimensions",
  xAxis.legend = "Sepal Length",
  yAxis.legend = "Sepal Width",
  point.size = 2,
  color.palette = "Dark2"
)
```

---

### buildTable

Renders tables using `flextable`, `gt`, or `kableExtra` with various formatting options for different output formats such as HTML, PDF, and DOCX.

#### Usage:

```r
buildTable(
  .x,
  library = "gt",
  format = "html",
  ...
)
```

#### Arguments:

- **`.x`**: A data frame or data table to be rendered as a table.
- **`library`**: The library to be used for rendering. Options are `"flextable"`, `"gt"`, and `"kable"`.
- **`format`**: The output format. Options are `"html"`, `"pdf"`, and `"docx"`.
- **`...`**: Additional formatting parameters, such as font sizes, fonts, boldness, alignment, and line settings.

#### Example:

```r
library(smartReports)

# Render a table using gt library for HTML format
buildTable(
  iris,
  library = "gt",
  format = "html",
  font.size.header = 14,
  font.size.body = 12,
  font.family.header = "Arial",
  font.family.body = "Arial",
  font.bold.header = TRUE,
  vlines.show = FALSE,
  hlines.show = TRUE,
  align.header = "center",
  align.body = "left"
)
```

---

### buildReport

Generates comprehensive reports in various formats (HTML, PDF, Word) by integrating plots, tables, and narrative text using `rmarkdown`.

#### Usage:

```r
buildReport(
  home_dir = ".",
  build_dir = "_build",
  index_filename = "index.qmd",
  quarto_filename = "_quarto.yml",
  language = "EN",
  output_format = c("html", "docx"),
  extensions = c("spl", "bst", "cls", "md", "aux", "log", "tex", "jpg", "sty", "docx", "pdf", "html"),
  postRender = TRUE
)
```

#### Arguments:

- **`home_dir`**: Path to the home directory.
- **`build_dir`**: Name of the build directory.
- **`index_filename`**: Name of the main Quarto index file (e.g., `"index.qmd"`).
- **`quarto_filename`**: Name of the Quarto configuration file (e.g., `"_quarto.yml"`).
- **`language`**: Language code (e.g., `"EN"`, `"ES"`).
- **`output_format`**: Output formats to render (e.g., `c("html", "docx", "pdf")`).
- **`extensions`**: File extensions to be removed during post-render cleaning.
- **`postRender`**: Logical. Should post-render cleaning be performed?

#### Example:

```r
library(smartReports)

# Generate report
buildReport(
  home_dir = ".",
  build_dir = "_build",
  index_filename = "index.qmd",
  language = "EN",
  output_format = c("html", "pdf")
)
```

---

## Feature Highlights

### Plot Customization (`buildPlot`)

- **Plot Types**: Line, spline, point, column, bar, and more.
- **Customization**: Titles, subtitles, dimensions, axis labels, color palettes, styles, and themes.
- **Scales**: Logarithmic scales and axis reversals.
- **Legends**: Layouts, alignments, visibility toggles.
- **Fonts**: Customize font sizes for axes legends, group legends, plot titles, and subtitles.
- **Advanced Options**: Fill polygons, display maximum absolute values, point markers, data labels.

### Table Formatting (`buildTable`)

- **Libraries Supported**: `flextable`, `gt`, `kableExtra`.
- **Fonts and Styles**: Customize font sizes, families, boldness.
- **Lines**: Control vertical and horizontal line visibility, colors, and thickness.
- **Alignment**: Header and body text alignment options.

### Report Generation (`buildReport`)

- **Formats**: HTML, PDF, Word.
- **Content Integration**: Easily include plots, tables, and text.
- **Templates**: Use custom or existing Quarto templates.
- **Automation**: Streamline report creation for reproducible research.
- **Language Support**: Generate reports in different languages.
- **Post-Render Cleaning**: Automatically remove unnecessary files after rendering.

---

## License

This package is licensed under the GPL-3.0 License.

---

## Contributing

Contributions are welcome! If you would like to contribute to `smartReports`, please follow these steps:

1. **Fork the Repository**: Click the "Fork" button at the top right of the repository page.
2. **Create a New Branch**: Use the command `git checkout -b feature/YourFeatureName`.
3. **Make Your Changes**: Implement your feature or bug fix.
4. **Commit Your Changes**: Use descriptive commit messages.
5. **Push to Your Fork**: `git push origin feature/YourFeatureName`.
6. **Submit a Pull Request**: Go to your fork on GitHub and open a pull request.

For major changes, please open an issue first to discuss what you would like to change.

---

## Contact

For any questions or issues, please contact the maintainer:

- **Name**: Alejandro Verri Kozlowski
- **Email**: [averri@fi.uba.ar](mailto:averri@fi.uba.ar)

---

**Note**: This README provides a brief overview of `smartReports`. For detailed documentation and additional examples, please refer to the official documentation site or the GitHub repository.
