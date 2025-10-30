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


