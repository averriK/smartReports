#' Generate rotating typewriter effect from multiple text files or text strings
#'
#' @param filePaths Vector of paths to .txt files with ASCII content (optional if texts is provided)
#' @param texts Vector of text strings to rotate through (optional if filePaths is provided)
#' @param speed Speed in milliseconds per character (default 5)
#' @param rotateDelay Delay in milliseconds before rotating to next file (default 3000)
#' @param font Font name: "vt323", "ibm", "courier", "space", "anonymous", "press", "silkscreen", "atari", "c64", "dotgothic", "overpass", "nova", "syne", "orbitron", "electrolize", "printchar21", "prnumber3", "data70" (default "vt323")
#' @param fontSize Font size in em units (default 0.9)
#' @param color Text color in hex (default "#00ff00")
#' @param bgColor Background color in hex (default "#000")
#' @param terminalWidth Terminal width in columns: 40, 60, 80, or NULL for auto (800px). Default NULL.
#' @return Prints HTML with rotating typewriter effect
#' @export
rotateTypewriter <- function(filePaths = NULL,
                             texts = NULL,
                             speed = 5,
                             rotateDelay = 3000,
                             font = "vt323",
                             fontSize = 0.9,
                             color = "#00ff00",
                             bgColor = "#000",
                             terminalWidth = NULL) {

  # Validate input: either texts or filePaths must be provided
  if (is.null(texts) && is.null(filePaths)) {
    stop("Either 'texts' or 'filePaths' must be provided")
  }
  
  if (!is.null(texts) && !is.null(filePaths)) {
    warning("Both 'texts' and 'filePaths' provided; using 'texts' and ignoring 'filePaths'")
  }

  # Determine content source
  contents <- list()
  
  if (!is.null(texts)) {
    # Use text strings directly
    if (length(texts) == 0) {
      stop("texts must contain at least one string")
    }
    for (text in texts) {
      # Trim leading and trailing whitespace/newlines
      content <- sub("^[\\s\\n]+", "", text)
      content <- sub("[\\s\\n]+$", "", content)
      contents[[length(contents) + 1]] <- content
    }
  } else {
    # Read from files
    if (length(filePaths) == 0) {
      stop("filePaths must contain at least one file path")
    }
    for (filePath in filePaths) {
      if (!file.exists(filePath)) {
        stop(paste("File not found:", filePath))
      }
      contents[[length(contents) + 1]] <- paste(readLines(filePath, warn = FALSE), collapse = "\n")
    }
  }

  # Generate unique ID
  id <- paste0("typewriter-rotate-", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6")))

  # Font configuration
  fontConfig <- switch(tolower(font),
    "vt323" = list(
      family = "'VT323', monospace",
      link = "https://fonts.googleapis.com/css2?family=VT323&display=swap"
    ),
    "ibm" = list(
      family = "'IBM Plex Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;700&display=swap"
    ),
    "courier" = list(
      family = "'Courier Prime', monospace",
      link = "https://fonts.googleapis.com/css2?family=Courier+Prime:wght@400;700&display=swap"
    ),
    "space" = list(
      family = "'Space Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&display=swap"
    ),
    "anonymous" = list(
      family = "'Anonymous Pro', monospace",
      link = "https://fonts.googleapis.com/css2?family=Anonymous+Pro:wght@400;700&display=swap"
    ),
    "press" = list(
      family = "'Press Start 2P', monospace",
      link = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap"
    ),
    "silkscreen" = list(
      family = "'Silkscreen', monospace",
      link = "https://fonts.googleapis.com/css2?family=Silkscreen:wght@400;700&display=swap"
    ),
    "atari" = list(
      family = "'Share Tech Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=Share+Tech+Mono&display=swap"
    ),
    "c64" = list(
      family = "'Pixelify Sans', monospace",
      link = "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;700&display=swap"
    ),
    "dotgothic" = list(
      family = "'DotGothic16', monospace",
      link = "https://fonts.googleapis.com/css2?family=DotGothic16&display=swap"
    ),
    "overpass" = list(
      family = "'Overpass Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=Overpass+Mono:wght@400;700&display=swap"
    ),
    "nova" = list(
      family = "'Nova Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=Nova+Mono&display=swap"
    ),
    "syne" = list(
      family = "'Syne Mono', monospace",
      link = "https://fonts.googleapis.com/css2?family=Syne+Mono&display=swap"
    ),
    "orbitron" = list(
      family = "'Orbitron', sans-serif",
      link = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap"
    ),
    "electrolize" = list(
      family = "'Electrolize', sans-serif",
      link = "https://fonts.googleapis.com/css2?family=Electrolize&display=swap"
    ),
    "printchar21" = list(
      family = "'Print Char 21', monospace",
      link = NULL,
      local = "PrintChar21"
    ),
    "prnumber3" = list(
      family = "'PR Number 3', monospace",
      link = NULL,
      local = "PRNumber3"
    ),
    "data70" = list(
      family = "'Data70', monospace",
      link = NULL,
      local = "data70"
    ),
    # Default to vt323
    list(
      family = "'VT323', monospace",
      link = "https://fonts.googleapis.com/css2?family=VT323&display=swap"
    )
  )

  # Build CSS style
  # Use !important for local fonts to override revealjs CSS
  font_decl <- if (!is.null(fontConfig$local)) {
    sprintf("font-family: %s !important", fontConfig$family)
  } else {
    sprintf("font-family: %s", fontConfig$family)
  }
  
  # Calculate width based on terminalWidth (columns) or use default
  width_css <- if (!is.null(terminalWidth)) {
    if (!terminalWidth %in% c(40, 60, 80)) {
      warning("terminalWidth should be 40, 60, or 80. Using 80.")
      terminalWidth <- 80
    }
    char_width <- fontSize * 0.6
    content_width <- terminalWidth * char_width
    sprintf("width: %.2fem", content_width + 4)
  } else {
    "width: 800px"
  }
  
  style <- sprintf(
    "%s; white-space: pre; font-size: %sem; color: %s; background-color: %s; padding: 20px; border-radius: 5px; %s; margin: 0 auto;",
    font_decl, fontSize, color, bgColor, width_css
  )

  # Escape all contents for JavaScript
  escaped_contents <- sapply(contents, function(content) {
    content_escaped <- gsub("\\\\", "\\\\\\\\", content)
    content_escaped <- gsub("\n", "\\\\n", content_escaped)
    content_escaped <- gsub("\r", "\\\\r", content_escaped)
    content_escaped <- gsub("`", "\\\\`", content_escaped)
    content_escaped <- gsub("\\$", "\\\\$", content_escaped)
    return(content_escaped)
  })

  # Generate JavaScript array of texts
  texts_js <- paste0("[`", paste(escaped_contents, collapse = "`,`"), "`]")

  # Generate HTML with font preload or local @font-face
  if (!is.null(fontConfig$link)) {
    cat('<link rel="preconnect" href="https://fonts.googleapis.com">\n')
    cat('<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>\n')
    cat(sprintf('<link href="%s" rel="stylesheet">\n\n', fontConfig$link))
  } else if (!is.null(fontConfig$local)) {
    # Local font - expects font to be installed on system
    # No @font-face needed, browser will use system font
  }

  cat(sprintf('\n<div id="%s"></div>\n\n', id))
  cat('<script>\n')
  cat('(function() {\n')
  cat(sprintf('  const texts = %s;\n', texts_js))
  cat(sprintf('  const containerId = "%s";\n', id))
  cat(sprintf('  const speed = %d;\n', speed))
  cat(sprintf('  const rotateDelay = %d;\n\n', rotateDelay))
  cat('  let currentIndex = 0;\n')
  cat('  let charIndex = 0;\n')
  cat('  let timeoutId = null;\n')
  cat('  let rotateTimeoutId = null;\n')
  cat('  let isActive = false;\n\n')

  cat('  function typeChar() {\n')
  cat('    const container = document.getElementById(containerId);\n')
  cat('    if (!container || !isActive) return;\n')
  cat('    const currentText = texts[currentIndex];\n')
  cat('    if (charIndex < currentText.length) {\n')
  cat('      container.textContent += currentText.charAt(charIndex);\n')
  cat('      charIndex++;\n')
  cat('      timeoutId = setTimeout(typeChar, speed);\n')
  cat('    } else {\n')
  cat('      rotateTimeoutId = setTimeout(rotateText, rotateDelay);\n')
  cat('    }\n')
  cat('  }\n\n')

  cat('  function rotateText() {\n')
  cat('    if (!isActive) return;\n')
  cat('    currentIndex = (currentIndex + 1) % texts.length;\n')
  cat('    charIndex = 0;\n')
  cat('    const container = document.getElementById(containerId);\n')
  cat('    if (container) {\n')
  cat('      container.textContent = "";\n')
  cat('      setTimeout(typeChar, 300);\n')
  cat('    }\n')
  cat('  }\n\n')

  cat('  function startTypewriter() {\n')
  cat('    const container = document.getElementById(containerId);\n')
  cat('    if (!container) return;\n')
  cat('    const slide = container.closest(".slide");\n')
  cat('    if (!slide || !slide.classList.contains("present")) return;\n')
  cat('    isActive = true;\n')
  cat('    currentIndex = 0;\n')
  cat('    charIndex = 0;\n')
  cat('    container.textContent = "";\n')
  cat('    if (timeoutId) clearTimeout(timeoutId);\n')
  cat('    if (rotateTimeoutId) clearTimeout(rotateTimeoutId);\n')
  cat('    setTimeout(typeChar, 300);\n')
  cat('  }\n\n')

  cat('  function stopTypewriter() {\n')
  cat('    isActive = false;\n')
  cat('    if (timeoutId) clearTimeout(timeoutId);\n')
  cat('    if (rotateTimeoutId) clearTimeout(rotateTimeoutId);\n')
  cat('  }\n\n')

  cat('  if (document.readyState === "loading") {\n')
  cat('    document.addEventListener("DOMContentLoaded", initTypewriter);\n')
  cat('  } else {\n')
  cat('    initTypewriter();\n')
  cat('  }\n\n')
  cat('  function initTypewriter() {\n')
  cat('    if (typeof Reveal !== "undefined") {\n')
  cat('      Reveal.on("slidechanged", function(event) {\n')
  cat('        const container = document.getElementById(containerId);\n')
  cat('        if (!container) return;\n')
  cat('        const slide = container.closest(".slide");\n')
  cat('        if (slide && slide.classList.contains("present")) {\n')
  cat('          startTypewriter();\n')
  cat('        } else {\n')
  cat('          stopTypewriter();\n')
  cat('        }\n')
  cat('      });\n')
  cat('      setTimeout(startTypewriter, 500);\n')
  cat('    } else {\n')
  cat('      setTimeout(initTypewriter, 100);\n')
  cat('    }\n')
  cat('  }\n')
  cat('})();\n')
  cat('</script>\n\n')
  cat(sprintf('<style>\n#%s { %s }\n</style>\n\n', id, style))
}
