% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildReport.R
\name{buildReport}
\alias{buildReport}
\title{Build Quarto Report}
\usage{
buildReport(
  build_dir = "_build",
  publish_dir = "_publish",
  index_filename = "index.qmd",
  quarto_filename = "_quarto.yml",
  language = "EN",
  output_format = c("html"),
  extensions = c("spl", "bst", "cls", "md", "aux", "log", "tex", "jpg", "sty", "docx",
    "pdf", "html"),
  render = TRUE
)
}
\arguments{
\item{build_dir}{Name of the yml folder.}

\item{publish_dir}{Name of the publish folder.}

\item{index_filename}{Name of the index file.}

\item{quarto_filename}{Name of the Quarto configuration file.}

\item{language}{Language code ('EN', 'ES', etc.).}

\item{output_format}{Output formats (e.g., 'html', 'docx').}

\item{extensions}{Extensions to be removed.}

\item{render}{Logical. Should the Quarto document be rendered?}
}
\description{
This function builds the Quarto report, handling pre-render and post-render tasks.
}
