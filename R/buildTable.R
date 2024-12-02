#' Render a table with specified formatting options
#'
#' This function renders a table using the specified library and formatting options.
#' It supports three libraries: flextable, gt, and kable.
#'
#' @param .x A data frame or data table to be rendered as a table.
#' @param library The library to be used for rendering. Options are "flextable", "gt", and "kable".
#' @param format The output format. Options are "html", "pdf", and "docx".
#' @param font.size.header The font size for the table header.
#' @param font.size.body The font size for the table body.
#' @param font.family.header The font family for the table header.
#' @param font.family.body The font family for the table body.
#' @param font.bold.header Logical. Whether to bold the header font.
#' @param font.bold.body Logical. Whether to bold the body font.
#' @param font.bold.all Logical. Whether to bold all text. Overrides font.bold.header and font.bold.body.
#' @param font.size.all Numeric. Font size for all text. Overrides font.size.header and font.size.body.
#' @param font.family.all Character. Font family for all text. Overrides font.family.header and font.family.body.
#' @param vlines.show Logical. Whether to show vertical lines.
#' @param hlines.show Logical. Whether to show horizontal lines.
#' @param vlines.color Color of vertical lines.
#' @param hlines.color Color of horizontal lines.
#' @param vlines.size Thickness of vertical lines.
#' @param hlines.size Thickness of horizontal lines.
#' @param align.header Alignment of header text. Options are "center", "left", "right".
#' @param align.body Alignment of body text. Options are "center", "left", "right".
#' @param caption The table caption.

#' @return A formatted table rendered using the specified library and options.
#' @examples
#'
#' iris |> buildTable(library="gt")
#' iris |> buildTable(library="flextable")
#' iris |> buildTable(library="kable")
#'
#' @importFrom data.table as.data.table data.table is.data.table
#' @importFrom flextable flextable fontsize font bold align vline hline autofit width
#' @importFrom gt gt tab_options tab_style cell_text cells_column_labels cells_body cell_borders px pct
#' @importFrom kableExtra kable kable_styling row_spec column_spec
#' @importFrom officer fp_border
#' @export buildTable
#'
buildTable <- function(.x,
                       library = "gt",
                       format = "html",
                       font.size.header = 14,
                       font.size.body = 12, font.family.header = "Arial", font.family.body = "Arial", caption=NULL,
                        font.bold.header = TRUE, font.bold.body = FALSE, font.bold.all = NULL,
                        font.size.all = NULL, font.family.all = NULL,
                        vlines.show = FALSE, hlines.show = TRUE,
                        vlines.color = "grey", hlines.color = "grey",
                        vlines.size = 1, hlines.size = 1,
                        align.header = "center", align.body = "left") {

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
         "flextable" = .buildTable.ft(.x, format, font.size.header, font.size.body, font.family.header, font.family.body, font.bold.header, font.bold.body,vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size, align.header, align.body,caption),
         "gt" = .buildTable.gt(.x, format, font.size.header, font.size.body * 1.2, font.family.header, font.family.body, font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size, align.header, align.body,caption),
         "kable" = .buildTable.kable(.x, format, font.size.header, font.size.body, font.family.header, font.family.body, font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size, align.header, align.body,caption),
         stop("Unsupported library. Please use 'flextable', 'gt', or 'kable'.")
  )
}

.buildTable.ft <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body, font.bold.header, font.bold.body,vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size,align.header, align.body,caption) {

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

  # Adjust for PDF output
  if (format == "pdf") {
    TABLE <- flextable::autofit(TABLE)
  }

  # Adjust for DOCX output
  if (format == "docx") {
    TABLE <- flextable::width(TABLE, width = 1.0)
  }

  if(!is.null(caption)){
    TABLE <- TABLE |> flextable::set_caption(caption)
  }

  return(TABLE)
}

.buildTable.gt <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body, font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size, align.header, align.body,caption) {

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
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = gt::px(font.size.body), font = font.family.body, align = align.body),
      locations = gt::cells_body()
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

  if(!is.null(caption)){
    TABLE <- TABLE |> gt::tab_caption(caption)
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
        style = gt::cell_borders(sides = "left", color = vlines.color, weight = gt::px(vlines.size)),
        locations = list(gt::cells_body(), gt::cells_column_labels())
      )
  }
  if (hlines.show) {
    TABLE <- TABLE |>
      gt::tab_style(
        style = gt::cell_borders(sides = "top", color = hlines.color, weight = gt::px(hlines.size)),
        locations = gt::cells_column_labels()
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

  return(TABLE)
}

.buildTable.kable <- function(.x, format, font.size.header, font.size.body, font.family.header, font.family.body, font.bold.header, font.bold.body, vlines.show, hlines.show, vlines.color, hlines.color, vlines.size, hlines.size,align.header, align.body,caption) {

  # Font and style settings
  TABLE <- kableExtra::kable(.x, format = format, escape = FALSE) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center", font_size = font.size.body)

  if (font.bold.header) {
    TABLE <- TABLE |>
      kableExtra::row_spec(0, bold = TRUE, font_size = font.size.header, extra_css = paste0("font-family:", font.family.header, ";"))
  } else {
    TABLE <- TABLE |>
      kableExtra::row_spec(0, font_size = font.size.header, extra_css = paste0("font-family:", font.family.header, ";"))
  }

  TABLE <- TABLE |>
    kableExtra::row_spec(1:nrow(.x), extra_css = paste0("font-family:", font.family.body, "; text-align:", align.body, ";"))

  if (font.bold.body) {
    TABLE <- TABLE |>
      kableExtra::row_spec(1:nrow(.x), bold = TRUE)
  }

  # Borders
  if (vlines.show) {
    css_vlines <- paste0("border-left: ", vlines.size, "px solid ", vlines.color, "; border-right: ", vlines.size, "px solid ", vlines.color, ";")
    TABLE <- TABLE |>
      kableExtra::column_spec(1, extra_css = css_vlines) |>
      kableExtra::column_spec(ncol(.x), extra_css = css_vlines)
  }

  if (hlines.show) {
    css_hlines <- paste0("border-top: ", hlines.size, "px solid ", hlines.color, "; border-bottom: ", hlines.size, "px solid ", hlines.color, ";")
    TABLE <- TABLE |>
      kableExtra::row_spec(0, extra_css = css_hlines)
  }

  return(TABLE)
}
