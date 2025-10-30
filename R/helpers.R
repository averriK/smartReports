#' Display text file as plain ASCII (non-formatted)
#'
#' @param filePath Path to the text file
#' @return Prints content using HTML pre tag for results: asis
#' @export
showASCII <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(paste("File not found:", filePath))
  }

  TEXT <- paste(readLines(filePath, warn = FALSE), collapse = "\n")
  TEXT <- gsub("&", "&amp;", TEXT)
  TEXT <- gsub("<", "&lt;", TEXT)
  TEXT <- gsub(">", "&gt;", TEXT)

  cat('\n<pre><code class="language-text">', sep = "")
  cat(TEXT)
  cat('</code></pre>\n\n')
}


#' Display markdown file as rendered HTML via iframe
#'
#' @param filePath Path to markdown file (will be rendered to HTML at runtime)
#' @param height Height of iframe (default "500px")
#' @param theme HTML theme (default "materia")
#' @param toc Show table of contents (default FALSE)
#' @return Prints iframe HTML using cat() for results: asis
#' @export
showMarkdownRendered <- function(filePath, height = "500px", theme = "spacelab", toc = FALSE) {
  if (!file.exists(filePath)) {
    stop(paste("File not found:", filePath))
  }

  HTML_PATH <- sub("\\.md$", ".html", filePath)
  
  # OPTIMIZATION: Only render if HTML doesn't exist or is older than MD
  SHOULD_RENDER <- !file.exists(HTML_PATH) || 
                   file.info(filePath)$mtime > file.info(HTML_PATH)$mtime
  
  if (SHOULD_RENDER) {
    message("[showMarkdownRendered] Rendering ", basename(filePath), " (HTML missing or outdated)")
    
    CONTENT <- readLines(filePath, warn = FALSE)

    BIB_FILE <- NULL
    if (file.exists("biblatex.bib")) {
      BIB_FILE <- normalizePath("biblatex.bib")
    } else if (file.exists("references.bib")) {
      BIB_FILE <- normalizePath("references.bib")
    }

    YAML_HEADER <- c(
      "---",
      "format:",
      "  html:",
      paste0("    theme: ", theme),
      "    highlight-style: tango",
      paste0("    toc: ", tolower(as.character(toc))),
      "    toc-depth: 4",
      "    number-sections: false",
      "    code-fold: true",
      "    code-tools: true",
      "    embed-resources: true"
    )

    if (!is.null(BIB_FILE)) {
      YAML_HEADER <- c(YAML_HEADER, paste0("bibliography: ", BIB_FILE))
    }

    YAML_HEADER <- c(YAML_HEADER, "---", "")

    if (!grepl("^---", CONTENT[1])) {
      CONTENT <- c(YAML_HEADER, CONTENT)
    }

    TEMP_FILE <- file.path(dirname(filePath), paste0("._temp_", basename(filePath)))
    writeLines(CONTENT, TEMP_FILE)

    QUARTO_CMD <- Sys.which("quarto")
    if (QUARTO_CMD == "") QUARTO_CMD <- "/usr/local/bin/quarto"

    system2(QUARTO_CMD, c("render", TEMP_FILE, "--to", "html"), stdout = FALSE, stderr = FALSE)

    TEMP_HTML <- sub("\\.md$", ".html", TEMP_FILE)
    if (file.exists(TEMP_HTML)) file.rename(TEMP_HTML, HTML_PATH)

    unlink(TEMP_FILE)
    TEMP_FILES_DIR <- sub("\\.md$", "_files", TEMP_FILE)
    if (dir.exists(TEMP_FILES_DIR)) unlink(TEMP_FILES_DIR, recursive = TRUE)
  } else {
    message("[showMarkdownRendered] Using cached HTML for ", basename(filePath))
  }

  cat('\n<iframe\n')
  cat('  src="', HTML_PATH, '"\n', sep = "")
  cat('  width="100%"\n')
  cat('  height="', height, '"\n', sep = "")
  cat('  style="border:0; max-width: 100%;"\n')
  cat('  loading="lazy"\n')
  cat('></iframe>\n\n')

  cat('<p style="font-size: 0.8em; margin-top: 5px; text-align: center;">\n')
  cat('<a href="', HTML_PATH, '" target="_blank">Open in new window</a>\n', sep = "")
  cat('</p>\n\n')
}


