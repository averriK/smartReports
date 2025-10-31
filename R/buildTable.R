#' Render a table with specified formatting options
#'
#' This function renders a table using the specified library and formatting options.
#' It supports three libraries: `flextable`, `gt`, and `kable`.
#'
#' @param .x A data frame or data table to be rendered as a table.
#' @param library The library to be used for rendering. Options are `"flextable"`, `"gt"`, and `"kable"`.
#' @param format The output format. Options are `"html"`, `"pdf"`, and `"docx"`.
#' @param font.size.header Numeric. Font size for the table header.
#' @param font.size.body Numeric. Font size for the table body.
#' @param font.family.header Character. Font family for the table header.
#' @param font.family.body Character. Font family for the table body.
#' @param font.bold.header Logical. Whether to bold the header font.
#' @param font.bold.body Logical. Whether to bold the body font.
#' @param font.bold.all Logical. If `TRUE`, bolds all text. Overrides `font.bold.header` and `font.bold.body`.
#' @param font.size.all Numeric. Font size for all text. Overrides `font.size.header` and `font.size.body`.
#' @param font.family.all Character. Font family for all text. Overrides `font.family.header` and `font.family.body`.
#' @param vlines.show Logical. Whether to show vertical lines.
#' @param hlines.show Logical. Whether to show horizontal lines.
#' @param vlines.color Character. Color of vertical lines.
#' @param hlines.color Character. Color of horizontal lines.
#' @param vlines.size Numeric. Thickness of vertical lines.
#' @param hlines.size Numeric. Thickness of horizontal lines.
#' @param align.header Character. Alignment of header text. Options are `"center"`, `"left"`, `"right"`.
#' @param align.body Character. Alignment of body text. Options are `"center"`, `"left"`, `"right"`.
#' @param caption Character. The table caption.
#' @return A formatted table rendered using the specified library and options.
#' @examples
#'
#' # Render a table using gt library for HTML format
#' buildTable(
#'   iris,
#'   library = "gt",
#'   format = "html",
#'   font.size.header = 14,
#'   font.size.body = 12,
#'   font.family.header = "Arial",
#'   font.family.body = "Arial",
#'   font.bold.header = TRUE,
#'   font.bold.body = FALSE,
#'   vlines.show = TRUE,
#'   hlines.show = TRUE,
#'   caption = "Iris Data Table"
#' )
#' @importFrom data.table as.data.table data.table is.data.table
#' @importFrom flextable flextable fontsize font bold align border set_caption
#' @importFrom gt gt tab_options tab_style cell_text cells_column_labels cells_body cell_borders px tab_caption
#' @importFrom kableExtra kable kable_styling row_spec column_spec
#' @importFrom officer fp_border
#' @export buildTable
#'
buildTable <- function(.x,
                       library = "gt",
                       format = "html",
                       font.size.header = 14,
                       font.size.body = 12,
                       font.family.header = "Arial",
                       font.family.body = "Arial",
                       caption = NULL,
                       font.bold.header = TRUE,
                       font.bold.body = FALSE,
                       font.bold.all = NULL,
                       font.size.all = NULL,
                       font.family.all = NULL,
                       vlines.show = FALSE,
                       hlines.show = TRUE,
                       vlines.color = "grey",
                       hlines.color = "grey",
                       vlines.size = 1,
                       hlines.size = 1,
                       align.header = "center",
                       align.body = "left") {

  # Override individual font sizes if font.size.all is specified
  if (!is.null(font.size.all)) {
    font.size.header <- font.size.all
    font.size.body <- font.size.all
  }

  # Override individual font families if font.family.all is specified
  if (!is.null(font.family.all)) {
    font.family.header <- font.family.all
    font.family.body <- font.family.all
  }

  # Override individual bold settings if font.bold.all is specified
  if (!is.null(font.bold.all)) {
    font.bold.header <- font.bold.all
    font.bold.body <- font.bold.all
  }

  # Ensure .x is a data.frame or data.table
  if (!is.data.frame(.x) && !is.data.table(.x)) {
    stop(".x must be a data.frame or data.table")
  }

  # Use switch to call appropriate rendering function
  switch(library,
         "flextable" = .buildTable.ft(.x, format, font.size.header, font.size.body, font.family.header,
                                      font.family.body, font.bold.header, font.bold.body, vlines.show,
                                      hlines.show, vlines.color, hlines.color, vlines.size, hlines.size,
                                      align.header, align.body, caption),
         "gt" = .buildTable.gt(.x, format, font.size.header, font.size.body, font.family.header,
                               font.family.body, font.bold.header, font.bold.body, vlines.show, hlines.show,
                               vlines.color, hlines.color, vlines.size, hlines.size, align.header, align.body, caption),
         "kable" = .buildTable.kable(.x, format, font.size.header, font.size.body, font.family.header,
                                     font.family.body, font.bold.header, font.bold.body, vlines.show,
                                     hlines.show, vlines.color, hlines.color, vlines.size, hlines.size,
                                     align.header, align.body, caption),
         stop("Unsupported library. Please use 'flextable', 'gt', or 'kable'.")
  )
}

# Internal functions for each library

.buildTable.ft <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body,
                           font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color,
                           vlines.size, hlines.size, align.header, align.body, caption) {

  TABLE <- flextable::flextable(.x)

  # Font settings
  TABLE <- flextable::fontsize(TABLE, size = font.size.header, part = "header")
  TABLE <- flextable::fontsize(TABLE, size = font.size.body, part = "body")
  TABLE <- flextable::font(TABLE, fontname = font.family.header, part = "header")
  TABLE <- flextable::font(TABLE, fontname = font.family.body, part = "body")

  # Bold settings
  if (font.bold.header) {
    TABLE <- flextable::bold(TABLE, part = "header")
  }
  if (font.bold.body) {
    TABLE <- flextable::bold(TABLE, part = "body")
  }

  # Alignment
  TABLE <- flextable::align(TABLE, align = align.header, part = "header")
  TABLE <- flextable::align(TABLE, align = align.body, part = "body")

  # Borders
  if (vlines.show) {
    TABLE <- flextable::vline(TABLE, border = officer::fp_border(color = vlines.color, width = vlines.size), part = "all")
  }
  if (hlines.show) {
    TABLE <- flextable::hline(TABLE, border = officer::fp_border(color = hlines.color, width = hlines.size), part = "header")
  }

  # Caption
  if (!is.null(caption)) {
    TABLE <- flextable::set_caption(TABLE, caption)
  }

  return(TABLE)
}

.buildTable.gt <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body,
                           font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color,
                           vlines.size, hlines.size, align.header, align.body, caption) {

  TABLE <- gt::gt(.x)

  # Font settings
  TABLE <- TABLE |>
    gt::tab_options(
      table.font.size = gt::px(font.size.body),
      table.font.names = font.family.body
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = gt::px(font.size.header), font = font.family.header),
      locations = gt::cells_column_labels()
    )

  # Bold settings
  if (font.bold.header) {
    TABLE <- TABLE |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )
  }
  if (font.bold.body) {
    TABLE <- TABLE |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body()
      )
  }

  # Alignment
  TABLE <- TABLE |>
    gt::tab_style(
      style = gt::cell_text(align = align.header),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = align.body),
      locations = gt::cells_body()
    )

  # Borders
  if (vlines.show) {
    TABLE <- TABLE |>
      gt::tab_style(
        style = gt::cell_borders(sides = c("left", "right"), color = vlines.color, weight = gt::px(vlines.size)),
        locations = gt::cells_body()
      )
  }
  if (hlines.show) {
    TABLE <- TABLE |>
      gt::tab_style(
        style = gt::cell_borders(sides = c("top", "bottom"), color = hlines.color, weight = gt::px(hlines.size)),
        locations = gt::cells_body()
      )
  }

  # Adjust for PDF output
  if (format == "pdf") {
    TABLE <- TABLE |>
      gt::tab_options(
        table.width = gt::pct(100)
      )
  }

  # Adjust for DOCX output
  if (format == "docx") {
    TABLE <- TABLE |>
      gt::tab_options(
        table.width = gt::pct(100)
      )
  }

  # Caption support
  if (!is.null(caption)) {
    TABLE <- TABLE |>
      gt::tab_caption(caption)
  }

  return(TABLE)
}

.buildTable.kable <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body,
                              font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color,
                              vlines.size, hlines.size, align.header, align.body, caption) {

  # Normalize format for knitr/kable
  fmt <- if (tolower(format) == "pdf") "latex" else format

  TABLE <- kableExtra::kable(.x, format = fmt, caption = caption, escape = FALSE)

  # Font and style settings
  TABLE <- TABLE |>
    kableExtra::kable_styling(full_width = FALSE, position = "center", font_size = font.size.body)

  # Header formatting
  TABLE <- TABLE |>
    kableExtra::row_spec(0, bold = font.bold.header, font_size = font.size.header, align = align.header, extra_css = paste0("font-family: ", font.family.header, ";"))

  # Body formatting
  TABLE <- TABLE |>
    kableExtra::row_spec(1:nrow(.x), bold = font.bold.body, align = align.body, extra_css = paste0("font-family: ", font.family.body, ";"))

  # Borders
  if (vlines.show || hlines.show) {
    border_styles <- paste0("border: ", hlines.size, "px solid ", hlines.color, ";")
    TABLE <- TABLE |>
      kableExtra::row_spec(0, extra_css = border_styles)
  }

  return(TABLE)
}
