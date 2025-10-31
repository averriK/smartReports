# smartReports

Professional report generation for R with interactive plots, formatted tables, Quarto YAML assembly, and revealjs typewriter effects.

IMPORTANT: This README documents only what is actually implemented in the source code (`R/*.R`, `NAMESPACE`). No speculative features are listed.

- Version: 0.3.3 (from DESCRIPTION)
- License: GPL (>= 3)
- R: >= 4.1.0

**Professional Report Generation for R**

R package for precise, reproducible report generation. Implements:
- Interactive Highcharts-based plots with robust fallbacks and validation
- Professionally formatted tables across gt/flextable/kable with format-aware styling
- Quarto YAML assembly and project scaffolding with language-aware configuration
- Vintage typewriter effects for revealjs presentations with Google/Local fonts

All features documented below reflect the actual code behavior in `R/*.R` with line references where appropriate.

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-0.3.3-green)](https://github.com/averriK/smartReports)
[![License](https://img.shields.io/badge/license-GPL--3.0-green.svg)](LICENSE)

## Contents

- Features (verified)
- Installation
- Quick Start
- Exported Functions (ground truth from NAMESPACE)
- Plotting API (buildPlot, buildPlot.Bar, buildPlot.Histogram, buildPlot.Model)
- Table API (buildTable)
- Quarto Integration (buildYAML)
- Presentation Utilities (showTypewriter, rotateTypewriter, buildIndexTypewriter)
- Viewers (showASCII, showMarkdownRendered)
- Algorithms and Implementation Details
  - Histogram binning (Freedman–Diaconis)
  - Density scaling relative to histogram
  - Color palette validation (grDevices::hcl.pals)
  - Arearange shading between two lines
  - YAML merge rules and resource detection
  - Revealjs integration and font handling
- Dependencies
- License and Citation

- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
  - [1. Create Interactive Plot](#1-create-interactive-plot)
  - [2. Build Formatted Table](#2-build-formatted-table)
- [3. Build YAML and Render with Quarto](#3-build-yaml-and-render-with-quarto)
- [Complete Workflow Example](#complete-workflow-example)
- [Core Functions](#core-functions)
- [Plot Types](#plot-types)
- [Table Formatting Options](#table-formatting-options)
- [Report Architecture](#report-architecture)
- [Use Cases](#use-cases)
- [Dependencies](#dependencies)
- [Project Structure](#project-structure)
- [License](#license)
- [Citation](#citation)
- [Author](#author)
- [Changelog](#changelog)

---

## Features (verified)

- Highcharts interactive plots with robust parameter validation and fallbacks
- Specialized plot builders: bar/column, histogram+density, model vs observations
- Format-aware tables via gt, flextable, and kable with unified styling params
- Quarto YAML assembly: merges _params.yml, _authors.yml, and format-specific files; language suffix support (e.g., _html_ES.yml)
- Revealjs typewriter effects with Google and local fonts; rotating multi-file animations
- Viewers: inline ASCII and rendered Markdown via iframe (auto-render with Quarto if stale)

- **Interactive Highcharter Plots**: Single `buildPlot()` function for line, spline, area, and scatter plots
- **Specialized Plot Types**: Bar charts, histograms, 2D/3D histograms (plotly), model comparison plots  
- **Multi-Library Table Support**: `buildTable()` works with flextable, gt, and kable
- **Quarto Integration**: `buildYAML()` assembles `_quarto.yml`; render with Quarto CLI or `quarto::quarto_render()`
- **Advanced Customization**: Fonts, colors, themes, logarithmic/reversed axes, legends, annotations
- **Format-Aware Tables**: Automatic styling adaptation for HTML, PDF, and DOCX outputs
- **Multi-Format Output**: Render HTML, PDF, Word via Quarto in a single step
- **Language Support**: Multi-language report generation with configurable templates
- **Interactive Exports**: Save highcharter plots as standalone HTML widgets
- **Arearange Shading**: Fill regions between line series with customizable opacity

---

## Installation

### From GitHub

```r
# Install devtools if needed
install.packages("devtools")

# Install smartReports from GitHub
devtools::install_github("averriK/smartReports")
```

### Dependencies

smartReports requires several packages that are automatically installed:

**Core:**
- `data.table`: Fast data manipulation
- `yaml`, `brio`: Configuration management

**Plotting:**
- `highcharter`: Interactive JavaScript charts (primary plotting library)
- `plotly`: 2D/3D histogram visualization (buildPlot.Hist2D, buildPlot.Hist3D)
- `htmlwidgets`, `webshot2`: Widget export

**Tables:**
- `gt`: Grammar of tables (best for HTML)
- `flextable`: Flexible tables (best for Word/PowerPoint)
- `kableExtra`: Enhanced kable tables
- `officer`: Office document manipulation

**System:**
- `grDevices`, `stats`, `graphics`: R base graphics utilities

---

## Quick Start

### 1. Create Interactive Plot

```r
library(smartReports)
library(data.table)

# Prepare data with ID, X, Y columns
# IMPORTANT: use data.lines for line plots, data.points for scatter plots
DT <- data.table(
  ID = rep(c("Series A", "Series B", "Series C"), each = 50),
  X = rep(seq(0, 10, length.out = 50), 3),
  Y = c(
    sin(seq(0, 10, length.out = 50)) + rnorm(50, 0, 0.1),
    cos(seq(0, 10, length.out = 50)) + rnorm(50, 0, 0.1),
    exp(-seq(0, 10, length.out = 50)/5) + rnorm(50, 0, 0.05)
  )
)

# Build interactive line plot
plot <- buildPlot(
  data.lines = DT,  # Use data.lines for lines, data.points for scatter
  line.type = "spline",
  plot.title = "Time Series Comparison",
  plot.subtitle = "Three experimental conditions",
  xAxis.legend = "Time (s)",
  yAxis.legend = "Amplitude",
  group.legend = "Condition",
  color.palette = "Dark 3",
  line.size = 2,
  xAxis.log = FALSE,
  yAxis.log = FALSE,
  legend.align = "right",
  legend.valign = "top"
)

# View in RStudio or browser
plot
```

### 2. Build Formatted Table

```r
# Prepare summary statistics
summary_table <- data.table(
  Species = c("setosa", "versicolor", "virginica"),
  Mean = c(5.01, 5.94, 6.59),
  SD = c(0.35, 0.52, 0.64),
  Min = c(4.3, 4.9, 4.9),
  Max = c(5.8, 7.0, 7.9),
  N = c(50, 50, 50)
)

# Render professional table
table <- buildTable(
  summary_table,
  library = "gt",
  format = "html",
  font.size.header = 14,
  font.size.body = 12,
  font.family.header = "Arial",
  font.family.body = "Arial",
  font.bold.header = TRUE,
  caption = "Sepal Length Statistics by Species",
  hlines.show = TRUE,
  vlines.show = FALSE,
  align.header = "center",
  align.body = "right"
)

table
```

### 3. Build YAML and Render with Quarto

```r
# Project structure:
# project/
#   ├── index.qmd           # Main report content
#   ├── _params.yml         # Report parameters (title, author, etc.)
#   ├── _authors.yml        # Author information
#   ├── _html.yml           # HTML format configuration
#   ├── _docx.yml           # Word format configuration (optional)
#   ├── _pdf.yml            # PDF format configuration (optional)
#   ├── references.bib      # Bibliography (optional)
#   └── _build/             # Support files (auto-created)

# 1) Build consolidated _quarto.yml and prepare folders
buildYAML(
  build_dir = "_build",
  publish_dir = "_publish",
  index_filename = "index.qmd",
  language = "EN",
  output_format = c("html", "docx", "pdf")
)

# 2) Render using Quarto (from R)
quarto::quarto_render(
  input = "index.qmd",
  output_format = c("html", "docx", "pdf")
)

# Output: _publish/index.html, _publish/index.docx, _publish/index.pdf
```

---

## Complete Workflow Example

```r
library(smartReports)
library(data.table)

# ============================================================================
# 1. DATA PREPARATION
# ============================================================================

# Example: Seismic hazard data
hazard_data <- data.table(
  ID = rep(c("Site A", "Site B", "Site C"), each = 30),
  Period = rep(10^seq(-2, 1, length.out = 30), 3),
  Acceleration = c(
    10^seq(-2, 0, length.out = 30) * rnorm(30, 1, 0.1),
    10^seq(-2, 0, length.out = 30) * rnorm(30, 1.2, 0.1),
    10^seq(-2, 0, length.out = 30) * rnorm(30, 0.8, 0.1)
  )
)

# Summary statistics
summary_stats <- hazard_data[, .(
  `Mean (g)` = round(mean(Acceleration), 3),
  `SD (g)` = round(sd(Acceleration), 3),
  `Max (g)` = round(max(Acceleration), 3),
  N = .N
), by = .(Site = ID)]

# ============================================================================
# 2. CREATE PLOTS
# ============================================================================

# Response spectrum plot (log-log scale)
spectrum_plot <- buildPlot(
  data.lines = hazard_data,
  line.type = "spline",
  plot.title = "Acceleration Response Spectra",
  plot.subtitle = "5% Damping Ratio",
  xAxis.legend = "Period (s)",
  yAxis.legend = "Spectral Acceleration (g)",
  group.legend = "Site",
  color.palette = "Viridis",
  line.size = 2,
  xAxis.log = TRUE,
  yAxis.log = TRUE,
  xAxis.min = 0.01,
  xAxis.max = 10,
  yAxis.min = 0.01,
  yAxis.max = 5,
  legend.layout = "vertical",
  legend.align = "right",
  legend.valign = "top",
  plot.height = 600,
  plot.width = 800
)

# Save as standalone HTML widget
htmlwidgets::saveWidget(spectrum_plot, "spectrum.html")

# ============================================================================
# 3. CREATE TABLES
# ============================================================================

# HTML version (for web reports)
html_table <- buildTable(
  summary_stats,
  library = "gt",
  format = "html",
  font.size.header = 14,
  font.size.body = 12,
  font.bold.header = TRUE,
  hlines.show = TRUE,
  hlines.color = "#CCCCCC",
  align.header = "center",
  align.body = "right"
)

# Word version (for client deliverables)
word_table <- buildTable(
  summary_stats,
  library = "flextable",
  format = "docx",
  font.size.all = 11,
  font.family.all = "Calibri",
  caption = "Table 1. Spectral Acceleration Statistics",
  hlines.show = TRUE,
  vlines.show = FALSE
)

# ============================================================================
# 4. SETUP REPORT PROJECT
# ============================================================================

# Create project structure
dir.create("report", showWarnings = FALSE)
dir.create("report/figures", showWarnings = FALSE)
dir.create("report/tables", showWarnings = FALSE)

# Save artifacts
saveRDS(spectrum_plot, "report/figures/spectrum_plot.rds")
saveRDS(html_table, "report/tables/summary_table.rds")

# Create index.qmd
writeLines(c(
  "---",
  "title: \"Seismic Hazard Analysis\"",
  "author: \"Engineering Team\"",
  "date: today",
  "---",
  "",
  "# Executive Summary",
  "",
  "This report presents acceleration response spectra for three sites.",
  "",
  "# Results",
  "",
  "```{r}",
  "#| echo: false",
  "spectrum_plot <- readRDS('figures/spectrum_plot.rds')",
  "spectrum_plot",
  "```",
  "",
  "```{r}",
  "#| echo: false",
  "summary_table <- readRDS('tables/summary_table.rds')",
  "summary_table",
  "```",
  "",
  "# Conclusions",
  "",
  "Spectral accelerations vary significantly across sites."
), "report/index.qmd")

# Create minimal _html.yml configuration
dir.create("report", showWarnings = FALSE)
writeLines(c(
  "format:",
  "  html:",
  "    toc: true",
  "    toc-depth: 2",
  "    theme: cosmo",
  "    code-fold: true"
), "report/_html.yml")

# ============================================================================
# 5. RENDER REPORT
# ============================================================================

setwd("report")

# Build _quarto.yml and prepare folders
buildYAML(
  build_dir = "_build",
  publish_dir = "_publish",
  index_filename = "index.qmd",
  language = "EN",
  output_format = c("html")
)

# Render with Quarto
quarto::quarto_render(
  input = "index.qmd",
  output_format = c("html")
)

# Output files:
#   _publish/index.html
```

---

## Exported Functions (ground truth)

From NAMESPACE, the following are exported:

- buildIndexTypewriter
- buildPlot
- buildPlot.Bar
- buildPlot.Hist2D (2D heatmaps/contours with plotly)
- buildPlot.Hist3D (3D bar histograms with plotly)
- buildPlot.Histogram
- buildPlot.Model
- buildTable
- buildYAML
- rotateTypewriter
- showASCII
- showMarkdownRendered
- showTypewriter

### Plotting Functions

| Function | Description | Input | Output |
|----------|-------------|-------|--------|
| `buildPlot` | Interactive line/scatter plots | data.table with ID, X, Y | highchart object |
| `buildPlot.Bar` | Bar/column charts | data.table with ID, X, Y | highchart object |
| `buildPlot.Histogram` | 1D histograms with optional density | data.table with ID, X | highchart object |
| `buildPlot.Model` | Model comparison plots | data.lines, data.points | highchart object |
| `buildPlot.Hist2D` | 2D heatmaps/contour plots | data.table with X, Y, Z | plotly object |
| `buildPlot.Hist3D` | 3D bar histograms | data.table with X, Y, Z | plotly object |

**Key parameters for buildPlot:**
- `data.lines`: data.table with ID, X, Y for line series (optional: style, fill)
- `data.points`: data.table with ID, X, Y for scatter points (optional: style)
- `line.type`: "line" or "spline"
- `color.palette`: Any palette from `grDevices::hcl.pals()`
- `xAxis.log`, `yAxis.log`: Logarithmic scales
- `xAxis.reverse`, `yAxis.reverse`: Axis inversion
- `plot.save`: Export to HTML file
- `plot.theme`: Highcharts theme object
- `fill.opacity`: Arearange shading opacity (0-1) when using `data.lines$fill`

### Tables

| Function | Description | Libraries | Output |
|----------|-------------|-----------|--------|
| `buildTable` | Formatted tables | gt, flextable, kableExtra | Formatted table object |

**Format-specific behavior:**
- **HTML**: Uses `gt` by default, supports CSS styling
- **DOCX**: Uses `flextable` for native Word compatibility
- **PDF**: Uses `kableExtra` with LaTeX styling

**Key parameters:**
- `library`: "gt" (HTML), "flextable" (Word), "kable" (LaTeX)
- `format`: "html", "docx", "pdf"
- `font.*`: Font families, sizes, bold
- `*lines.show`, `*lines.color`, `*lines.size`: Border control
- `align.header`, `align.body`: Text alignment
- `caption`: Table caption/title

### Reports

| Function | Description | Input | Output |
|----------|-------------|-------|--------|
| `buildYAML` | Assemble `_quarto.yml` and prepare folders | .qmd + config files | `_quarto.yml` + prepared dirs |

Render separately with Quarto: `quarto::quarto_render("index.qmd", output_format = c("html","pdf","docx"))`.

Note: When using `library = "kable"`, you may pass `format = "pdf"`; internally it maps to LaTeX.

**Configuration files:**
- `_params.yml`: Report parameters (title, date, custom variables)
- `_authors.yml`: Author metadata (name, affiliation, ORCID)
- `_html.yml`: HTML-specific options (TOC, CSS, Bootstrap theme)
- `_pdf.yml`: PDF-specific options (LaTeX engine, geometry, fonts)
- `_docx.yml`: Word-specific options (reference.docx template)
- `references.bib`: Bibliography (BibTeX format)

**Key parameters:**
- `build_dir`: Temporary build folder
- `publish_dir`: Final output folder  
- `language`: "EN", "ES", etc. (selects templates)
- `output_format`: c("html", "docx", "pdf")

---

## Plot Types

### Line and Spline Plots (buildPlot)

**Features:**
- Interactive tooltips, zooming, panning
- Responsive design for web reports
- Built-in export to PNG/SVG/PDF
- Logarithmic and reversed axes
- Arearange shading between lines

**Example:**
```r
# Simple line plot
buildPlot(
  data.lines = DT,
  line.type = "line",
  xAxis.log = FALSE,
  yAxis.log = FALSE
)

# Log-log plot with splines
buildPlot(
  data.lines = DT,
  line.type = "spline",
  xAxis.log = TRUE,
  yAxis.log = TRUE,
  color.palette = "Set1"
)

# With arearange shading (requires 'fill' column in data.lines)
DT[ID %in% c("Upper", "Lower"), fill := TRUE]
buildPlot(
  data.lines = DT,
  line.type = "spline",
  color.palette = "Viridis"
)
```

### Bar Charts (buildPlot.Bar)

**Features:**
- Grouped, stacked, or 100% stacked bars
- Categorical or numeric x-axis
- Customizable spacing and borders

**Example:**
```r
library(data.table)

bar_data <- data.table(
  ID = rep(c("2020", "2021", "2022"), each = 4),
  X = rep(c("Q1", "Q2", "Q3", "Q4"), 3),
  Y = c(23, 25, 28, 30, 25, 28, 31, 33, 28, 32, 35, 38)
)

buildPlot.Bar(
  bar_data,
  plot.title = "Quarterly Sales",
  xAxis.legend = "Quarter",
  yAxis.legend = "Revenue (M USD)",
  color.palette = "Dark 2",
  stacking = "normal"  # "none", "normal", or "percent"
)
```

### Histograms (buildPlot.Histogram)

**Features:**
- Automatic Freedman-Diaconis bin calculation
- Optional density overlay
- Quantile markers
- Log-scale support

**Example:**
```r
hist_data <- data.table(
  ID = "Sample",
  X = rnorm(1000, mean = 5, sd = 2)
)

buildPlot.Histogram(
  hist_data,
  plot.type = c("histogram", "density"),
  xAxis.log = FALSE,
  plot.markers = TRUE,  # Add quantile markers
  color.palette = "Dark3"
)
```

### Model Comparison (buildPlot.Model)

**Features:**
- Dual data sources (lines for models, points for observations)
- Log-log axes by default
- Specialized for model fit visualization

**Example:**
```r
model_lines <- data.table(
  ID = "Prediction",
  X = 10^seq(-2, 2, length.out = 100),
  Y = 10^(0.5 * seq(-2, 2, length.out = 100))
)

observed_points <- data.table(
  ID = "Observations",
  X = 10^runif(50, -2, 2),
  Y = 10^(0.5 * runif(50, -2, 2) + rnorm(50, 0, 0.2))
)

buildPlot.Model(
  data.lines = model_lines,
  data.points = observed_points,
  xAxis.legend = "Input",
  yAxis.legend = "Output"
)
```

---

## Table Formatting Options

### Global Font Control

Apply consistent styling across header and body:

```r
buildTable(
  iris,
  font.size.all = 12,
  font.family.all = "Times New Roman",
  font.bold.all = FALSE
)
```

### Border Customization

Fine-tune table borders:

```r
buildTable(
  iris,
  hlines.show = TRUE,
  hlines.color = "#2C3E50",
  hlines.size = 2,
  vlines.show = FALSE
)
```

### Format-Specific Optimization

```r
# HTML: Use gt
buildTable(iris, library = "gt", format = "html")

# Word: Native .docx formatting with flextable
buildTable(iris, library = "flextable", format = "docx")

# PDF: LaTeX booktabs style with kable
buildTable(iris, library = "kable", format = "pdf")
```

---

## Algorithms and Implementation Details

### buildPlot (R/buildPlot.R)
- Accepts data.lines and/or data.points with columns ID, X, Y; validates numeric Y; warns and falls back for invalid inputs
- Color mapping via grDevices::hcl.colors over all unique IDs; palette validated against hcl.pals; fallback to a default palette
- Lines: per-series dashStyle from style field with mapping; per-series type (line/spline); legend with title and font sizes
- Arearange shading: if exactly two IDs in data.lines have fill == TRUE, builds an arearange between interpolated curves (approx with method = interpolation.method)
- Tooltips: basic X/Y display; export enabled with PNG/JPEG/PDF/SVG/CSV/XLS

### buildPlot.Bar (R/buildPlot.Bar.R)
- Data: columns X, Y, ID; X can be factor (categorical) or numeric
- Stacking modes: none/grouped, normal/stacked, percent/100%
- Palette: vector or hcl.pals name; per-series color assignment
- Axis labels, legend placement, and spacing parameters (groupPadding, pointPadding) supported

### buildPlot.Histogram (R/buildPlot.Histogram.R)
- Data: columns X (numeric), ID (factor/character)
- plot.type: histogram and/or density
- Binning: Freedman–Diaconis rule when histogram.breaks = NULL; supports log-scale by computing breaks in log10-space
- Density: scaled to histogram peak so both overlays are comparable; computed even if only density is plotted
- Quantile markers: optional series at probabilities c(0.05, 0.10, 0.16, 0.50, 0.84, 0.90, 0.95)

### buildPlot.Model (R/buildPlot.Model.R)
- Dual inputs: lines (spline series) and points (scatter), default log-log axes
- Legend vertical on the right; basic styling and sizes

### buildTable (R/buildTable.R)
- Unified API for gt, flextable, and kable with consistent font controls
- Global overrides: font.size.all, font.family.all, font.bold.all override per-part settings
- Borders: vlines/hlines show, color, size; applied via each library’s API
- Caption support across libraries

### buildYAML (R/buildYAML.R)
- Merges configuration files:
  - Authors: _authors.yml (validated authors; removes author from _params when _authors exists)
  - Parameters: _params_LANG.yml fallback _params.yml
  - Formats: _html_LANG.yml or _html.yml (and similarly for revealjs, docx, pdf)
  - Elsevier: _els-pdf.yml optional
- Resources detection and injection:
  - bibliography: references.bib (or biblatex.bib)
  - logo.png: sets format.html.logo or format.revealjs.logo
  - CSS files: added to format.html.css and format.revealjs.css after copying into build_dir
- Output directories: ensures clean publish_dir; copies package extdata resources into build_dir; writes consolidated _quarto.yml
- Does not call Quarto render directly; user should run quarto::quarto_render()

### Typewriter utilities
- showTypewriter: prints a single text with JS typewriter effect; supports multiple Google/local fonts; Revealjs-aware init
- rotateTypewriter: cycles through multiple files; Revealjs-aware lifecycle; speed and rotateDelay control
- buildIndexTypewriter: builds a numbered index text (>> MODULE INDEX, [01] -- NAME) and calls showTypewriter

### Viewer Utilities (R/showASCII.R, R/showMarkdownRendered.R)

#### showASCII(filePath)
**Purpose:** Display raw text files as formatted code blocks in Quarto documents

**Logic:**
1. Reads text file with `readLines()`
2. Escapes HTML entities: `&` → `&amp;`, `<` → `&lt;`, `>` → `&gt;`
3. Wraps content in `<pre><code class="language-text">` tags
4. Outputs via `cat()` for `results: asis` chunks

**Use case:** Embedding log files, configuration files, or terminal output without rendering

**Example:**
```r
showASCII("logs/system.log")
```

#### showMarkdownRendered(filePath, height, theme, toc)
**Purpose:** Render Markdown files to HTML and embed as interactive iframe in Quarto documents

**Logic:**
1. **Staleness check:** Only re-renders if HTML is missing or `.md` is newer than `.html` (via `file.info()$mtime`)
2. **YAML injection:** If `.md` lacks YAML front matter, prepends minimal Quarto config:
   - `theme`: Bootstrap theme (default: "spacelab")
   - `toc`: Table of contents (default: FALSE)
   - `embed-resources: true` (self-contained HTML)
   - Auto-detects `references.bib` or `biblatex.bib` and adds `bibliography:` field
3. **Rendering:**
   - Writes temp file with `._temp_` prefix
   - Calls `quarto render` via `system2()`
   - Moves rendered HTML to final path
   - Cleans up temp files and `_files/` directory
4. **Embedding:**
   - Generates `<iframe>` with 100% width, configurable height
   - Adds "Open in new window" link below iframe
   - Uses `loading="lazy"` for performance

**Optimization:** Cached rendering avoids redundant Quarto calls (message: "Using cached HTML")

**Use case:** Embedding analysis reports, method descriptions, or supplementary documents

**Example:**
```r
# In a Quarto .qmd document with results: asis
showMarkdownRendered(
  "analysis/methods.md",
  height = "600px",
  theme = "cosmo",
  toc = TRUE
)
```

**Parameters:**
- `filePath`: Path to `.md` file
- `height`: iframe height (default "500px")
- `theme`: HTML theme for rendering (default "spacelab")
- `toc`: Show table of contents (default FALSE)

### File-Based Configuration

smartReports uses a modular configuration system:

```
project/
├── index.qmd              # Main content
├── _params.yml            # Report metadata
├── _authors.yml           # Author details
├── _html.yml              # HTML-specific settings
├── _pdf.yml               # PDF-specific settings (optional)
├── _docx.yml              # Word-specific settings (optional)
├── references.bib         # Bibliography (optional)
├── _build/                # Temp files (auto-cleaned)
│   ├── index.html
│   ├── index.pdf
│   └── *.aux, *.log, etc.
└── _publish/              # Final outputs
    ├── index.html
    ├── index.docx
    └── index.pdf
```

### Multi-Language Support

Specify language-specific templates:

```r
buildYAML(
  language = "ES",
  output_format = c("html", "pdf")
)
quarto::quarto_render("index.qmd", output_format = c("html", "pdf"))
```

Loads:
- `_params_ES.yml`
- `_html_ES.yml`
- `_pdf_ES.yml`

---

## Use Cases

- **Seismic Hazard Reports**: Acceleration spectra, hazard curves, site amplification plots
- **Engineering Analysis**: Structural response, displacement time-histories, capacity curves
- **Research Publications**: Multi-format output (HTML for web, PDF for journals, Word for preprints)
- **Client Deliverables**: Professional tables and interactive plots embedded in reports
- **Data Exploration**: Rapid prototyping of visualization + table combinations
- **Reproducible Research**: Version-controlled Quarto workflows with embedded R code
- **Technical Documentation**: API documentation, method descriptions with code examples
- **Dashboards**: Standalone HTML plots exported for embedding in web applications

---

## Dependencies

### Core Requirements

- **R 4.1.0+**: Required R version (per DESCRIPTION)
- **Quarto**: External CLI tool for rendering (install from https://quarto.org, NOT an R package dependency)

### R Package Dependencies

**Plotting (5 packages):**
- `highcharter`: JavaScript charts (primary)
- `plotly`: 3D plots
- `htmlwidgets`: Widget export
- `webshot2`: Screenshot generation
- `grDevices`, `graphics`, `stats`: Base R graphics

**Tables (4 packages):**
- `gt`: Grammar of tables
- `flextable`: Flexible table layouts
- `kableExtra`: Enhanced kable
- `officer`: Office formats

**Data (1 package):**
- `data.table`: Fast data manipulation

**Report (2 packages):**
- `yaml`: Configuration parsing
- `brio`: File I/O

### System Dependencies

- **Quarto CLI**: https://quarto.org/docs/get-started/
- **Pandoc**: Included with Quarto
- **LaTeX** (for PDF): TinyTeX recommended (`quarto install tinytex`)

---

## Project Structure

```
smartReports/
├── DESCRIPTION              # Package metadata
├── NAMESPACE                # Exported functions
├── LICENSE.md               # GPL-3.0
├── README.md                # This file
├── R/                       # Source code
│   ├── buildPlot.R         # Main plotting function
│   ├── buildPlot.Bar.R     # Bar chart specialization
│   ├── buildPlot.Histogram.R
│   ├── buildPlot.Model.R
│   ├── buildPlot.Hist2D.R  # 2D histograms (heatmaps/contours)
│   ├── buildPlot.Hist3D.R  # 3D bar histograms
│   ├── buildTable.R        # Table formatting
│   ├── buildYAML.R         # Build consolidated _quarto.yml
│   ├── export.R            # Export utilities
│   └── local.R             # Internal helpers
├── man/                     # Documentation (auto-generated)
├── inst/                    # Installed files
│   ├── extdata/            # Support files for reports
│   ├── examples/           # Usage examples
│   └── docx/               # Word styles
└── tests/                   # Unit tests (if available)
```

---

## License

GPL-3.0

Copyright (c) 2025 Alejandro Verri Kozlowski

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

---

## Citation

If you use this package in your research or professional work, please cite:

```bibtex
@software{smartReports2025,
  author = {Verri Kozlowski, Alejandro},
  title = {smartReports: Professional Report Generation for R},
  year = {2025},
  version = {0.3.3},
  url = {https://github.com/averriK/smartReports}
}
```

---

## Author

**Alejandro Verri Kozlowski**

- Email: averri@fi.uba.ar
- ORCID: [0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)
- GitHub: [@averriK](https://github.com/averriK)

**Affiliation:**
- Universidad de Buenos Aires, Facultad de Ingeniería

---

## Changelog

### Version 0.3.3 (Current)

**Plot Enhancements:**
- Refactored `buildPlot()` to support separate `data.lines` and `data.points` inputs
- Added `line.type` parameter: "line", "spline"
- Implemented area range shading via `fill` column in data.lines
- Improved color palette validation with fallback to Highcharts defaults
- Enhanced line style handling with solid/dash/dot/dashDot options
- Added interpolation method parameter for `approx()` customization
- Deprecated `library` and `plot.type` parameters (now triggers warnings)

**Table Improvements:**
- Enhanced global font controls (`font.size.all`, `font.family.all`, `font.bold.all`)
- Improved border customization (show/hide, color, size for h/v lines)
- Better format detection and library-specific rendering
- Caption support for gt, flextable, and kable

**Report Features:**
- Multi-language template support (_params_ES.yml, _html_EN.yml, etc.)
- Modular YAML configuration system (_params.yml, _authors.yml, _html.yml, _pdf.yml, _docx.yml)
- Bibliography integration (references.bib auto-detection)
- Author metadata validation and formatting
- Automatic logo and CSS file detection and inclusion
- New `buildYAML()` function to assemble `_quarto.yml` (render with Quarto)

**Bug Fixes:**
- Fixed axis log scale behavior
- Improved legend positioning and alignment
- Fixed table border rendering in DOCX format
- Corrected color palette fallback mechanism

**Breaking Changes:**
- `buildPlot()` now requires `data.lines` or `data.points` instead of `data`
- `library` and `plot.type` parameters deprecated (use `line.type` instead)
- Rendering now done via Quarto directly (use `quarto::quarto_render()` after `buildYAML()`)

### Version 0.3.0

- Initial public release
- Core functions: buildPlot, buildTable, buildYAML
- Support for highcharter plots
- gt, flextable, kableExtra table rendering
- Quarto integration

---

## Contributing

Issues and pull requests are welcome at the [GitHub repository](https://github.com/averriK/smartReports).

For bug reports, please include:
- R version and platform
- Package versions (`sessionInfo()`)
- Minimal reproducible example
- Expected vs actual output

For feature requests:
- Use case description
- Proposed API
- Example code

---

## Acknowledgments

Built on the work of many excellent R packages:
- Highcharts.js via `highcharter` by Joshua Kunst
- Grammar of Tables (`gt`) by RStudio
- `flextable` by David Gohel
- Quarto by Posit PBC
- data.table by Matt Dowle and Arun Srinivasan

---

## Notes

### About buildPlot.Hist2D and buildPlot.Hist3D

These functions are now EXPORTED and fully documented. Use them for 2D heatmap/contour plots and 3D bar histograms with plotly. See documentation with `?buildPlot.Hist2D` and `?buildPlot.Hist3D`.

### About AI/LLM Integration

**Important:** Despite what may be mentioned in older documentation or the DESCRIPTION file, this package does NOT currently include AI/LLM integration features. There is no OpenAI, Anthropic, or Google API integration implemented. The package focuses on visualization and report generation using R-native tools.

If you need text enhancement or translation capabilities, consider using external tools or packages designed for that purpose.

### Reporting Issues

If you find discrepancies between documentation and actual behavior, please file an issue at the GitHub repository with:
- What the documentation says
- What actually happens
- Minimal reproducible example

---

## Typewriter Effects for Presentations

### Overview

smartReports includes vintage terminal typewriter effects for Quarto revealjs presentations. Three main functions:

1. **`showTypewriter()`** - Single typewriter animation from text or file
2. **`rotateTypewriter()`** - Rotating animations through multiple files or text strings
3. **`buildIndexTypewriter()`** - Auto-generate chapter indexes with typewriter effect

### Available Vintage Fonts

**13 authentic vintage computer/terminal fonts:**

| Font | Description | Era | Color Recommendation |
|------|-------------|-----|---------------------|
| `vt323` | DEC VT320 terminal | 1987 | `#00ff00` (green phosphor) |
| `ibm` | IBM Plex Mono | 1970s-80s | `#ffb000` (amber) |
| `courier` | Typewriter | 1950s | `#000000` on beige |
| `space` | Retro-futuristic | 1960s-70s | `#00ffff` (cyan) |
| `anonymous` | Unix/Linux terminal | 1990s | `#00ff00` |
| `press` | 8-bit arcade/NES | 1980s | `#ffffff` |
| `silkscreen` | Pixel art display | 1980s-90s | `#ff00ff` |
| `atari` | Atari 800/ST | 1970s-80s | `#00ff00` |
| `c64` | Commodore 64 | 1980s | `#8888ff` on `#4040cc` |
| `dotgothic` | 16x16 CRT pixel | 1980s | `#00ff00` |
| `overpass` | OCR-style blocky | ID cards | `#000000` |
| `nova` | MOTHER/NORAD | 1970s-80s | `#a8b820` or `#ff8800` |
| `syne` | Military radar | 1970s | `#ff8800` (orange) |
| `data70` | OCR/ID cards | 1970s | `#ff0000` (local font) |

### Quick Examples

#### 1. Simple Typewriter from Text

```r
library(smartReports)

showTypewriter(
  text = ">> SYSTEM READY\n>> LOADING...",
  font = "vt323",
  speed = 10,
  color = "#00ff00",
  bgColor = "#000"
)
```

#### 2. Typewriter from File

```r
showTypewriter(
  filePath = "terminal_output.txt",
  font = "ibm",
  fontSize = 0.9,
  color = "#ffb000"
)
```

#### 3. Build Chapter Index

```r
buildIndexTypewriter(
  index = c("INTRODUCTION", "METHODS", "RESULTS", "CONCLUSIONS"),
  font = "atari",
  fontSize = 0.9,
  color = "#00ff00"
)
```

#### 4. Rotating Animations (from text strings - v0.4.1+)

```r
# Direct text strings (no files needed!)
rotateTypewriter(
  texts = c(
    ">> SYSTEM BOOTING...\n>> KERNEL LOADED\n>> MEMORY CHECK: OK",
    ">> DIAGNOSTICS RUNNING...\n>> ALL SYSTEMS NOMINAL",
    ">> USER LOGIN DETECTED\n>> ACCESS GRANTED\n>> WELCOME, COMMANDER"
  ),
  speed = 5,
  rotateDelay = 2000,
  font = "vt323",
  color = "#00ff00",
  bgColor = "#000"
)
```

#### 5. Rotating Animations (from files)

```r
# HAL 9000 style (2001: A Space Odyssey)
rotateTypewriter(
  filePaths = c(
    "hal_01.txt",
    "hal_02.txt",
    "hal_03.txt"
  ),
  speed = 3,
  rotateDelay = 2000,
  font = "ibm",
  fontSize = 0.7,
  color = "#88CCDD",
  bgColor = "#0a0a0a"
)

# NORAD War Room (WarGames 1983)
rotateTypewriter(
  filePaths = c("wopr_01.txt", "wopr_02.txt"),
  font = "nova",
  color = "#ff8800",  # Orange radar display
  bgColor = "#000000"
)

# MOTHER (Alien 1979)
rotateTypewriter(
  filePaths = c("mother_01.txt", "mother_02.txt"),
  font = "nova",
  color = "#a8b820",  # Green-yellow phosphor
  bgColor = "#0a0a0a"
)
```

### Terminal Width (v0.4.1+)

Emulate classic terminal column widths:

```r
# Commodore 64 - 40 columns
showTypewriter(
  text = ">> COMMODORE 64 BASIC V2\n>> 38911 BASIC BYTES FREE\n>> READY.",
  font = "c64",
  fontSize = 0.8,
  color = "#8888ff",
  bgColor = "#4040cc",
  terminalWidth = 40  # 40-column display
)

# DEC VT320 - 80 columns
showTypewriter(
  text = ">> DEC VT320 TERMINAL\n>> LOGIN: root",
  font = "vt323",
  fontSize = 0.9,
  color = "#00ff00",
  bgColor = "#000",
  terminalWidth = 80  # 80-column display
)

# IBM PC - 60 columns
rotateTypewriter(
  texts = c(
    ">> IBM PC XT\n>> ROM BASIC",
    ">> (C) IBM 1981"
  ),
  font = "ibm",
  fontSize = 0.85,
  color = "#ffb000",
  bgColor = "#000",
  terminalWidth = 60  # 60-column display
)

# Default: 800px (no terminalWidth specified)
```

**Supported widths:** 40, 60, 80 columns (auto-calculated based on fontSize)  
**Default:** NULL (uses fixed 800px width)

### Color Palettes

**Classic CRT Terminal Colors:**

```r
# Green phosphor (VT terminals)
color = "#00ff00", bgColor = "#000"

# Amber phosphor (DEC, IBM)
color = "#ffb000", bgColor = "#000"

# White phosphor
color = "#f0f0f0", bgColor = "#000"

# Cyan (Tektronix)
color = "#00ffff", bgColor = "#000"

# Commodore 64 blue
color = "#8888ff", bgColor = "#4040cc"

# MOTHER (Alien) - green-yellow
color = "#a8b820", bgColor = "#0a0a0a"

# NORAD radar - orange
color = "#ff8800", bgColor = "#000"
```

### Function Parameters

#### showTypewriter()

```r
showTypewriter(
  filePath = NULL,        # Path to .txt file
  text = NULL,            # Direct text (alternative to filePath)
  speed = 5,              # Milliseconds per character
  font = "vt323",        # Font name (see table above)
  fontSize = 0.9,         # Em units
  color = "#00ff00",     # Text color (hex)
  bgColor = "#000",      # Background color (hex)
  terminalWidth = NULL   # Terminal columns: 40, 60, 80, or NULL (800px)
)
```

#### rotateTypewriter()

```r
rotateTypewriter(
  filePaths = NULL,       # Vector of file paths (optional if texts provided)
  texts = NULL,           # Vector of text strings (optional if filePaths provided)
  speed = 5,              # Typing speed (ms/char)
  rotateDelay = 3000,     # Delay before rotating to next text/file (ms)
  font = "vt323",
  fontSize = 0.9,
  color = "#00ff00",
  bgColor = "#000",
  terminalWidth = NULL    # Terminal columns: 40, 60, 80, or NULL (800px)
)
```

#### buildIndexTypewriter()

```r
buildIndexTypewriter(
  index,                  # Character vector of chapter names
  speed = 5,
  font = "vt323",
  fontSize = 0.9,
  color = "#00ff00",
  bgColor = "#000"
)
```

### Using in Quarto Revealjs

```markdown
---
title: "My Presentation"
format: revealjs
---

```{r setup}
library(smartReports)
```

## Chapter I

```{r}
#| echo: false
#| results: asis
buildIndexTypewriter(
  index = c("INTRODUCTION", "METHODS", "RESULTS"),
  font = "atari",
  color = "#00ff00"
)
```

## Terminal Demo

```{r}
#| echo: false
#| results: asis
rotateTypewriter(
  filePaths = c("demo1.txt", "demo2.txt"),
  font = "vt323",
  color = "#00ff00"
)
```
```

### Adding Custom Local Fonts

For fonts not in Google Fonts (like Data70):

1. **Download font file** (`.ttf`, `.woff`, or `.woff2`)
2. **Place in** `smartReports/inst/fonts/`
3. **Add configuration** in R helpers (edit `typewriter.R`)

```r
"data70" = list(
  family = "'Data70', monospace",
  link = NULL,  # NULL = local font
  local = "data70"  # filename without extension
)
```

4. **Use in code:**

```r
showTypewriter(
  text = "ID: 4532-1523-9876",
  font = "data70"
)
```

See `inst/fonts/README.md` for detailed instructions.

### Font Installation

**Data70 (OCR-style):**

1. Download from: https://www.dafont.com/data70.font
2. Extract `data70.ttf`
3. Place in `smartReports/inst/fonts/`
4. (Optional) Convert to WOFF2:
   ```bash
   pip3 install fonttools brotli
   pyftsubset data70.ttf --output-file=data70.woff2 --flavor=woff2
   ```

### Notes

- All Google Fonts load automatically
- Local fonts require manual download
- Typewriter effects work in revealjs presentations only
- Effects trigger when slide becomes active
- Unique IDs prevent multiple animations interfering
- Speed recommendations: 3-5ms for smooth animation, 10-20ms for dramatic effect
