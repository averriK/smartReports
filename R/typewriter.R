#' Generate typewriter effect HTML from text file or direct text
#'
#' @param filePath Path to .txt file with ASCII content (optional if text is provided)
#' @param text Direct text content (optional if filePath is provided)
#' @param speed Speed in milliseconds per character (default 5)
#' @param font Font name: "vt323", "ibm", "courier", "space", "anonymous", "press", "silkscreen", "atari", "c64", "dotgothic", "overpass", "nova", "syne", "orbitron", "electrolize", "printchar21", "prnumber3", "data70" (default "vt323")
#' @param fontSize Font size in em units (default 0.9)
#' @param color Text color in hex (default "#00ff00")
#' @param bgColor Background color in hex (default "#000")
#' @return Prints HTML with typewriter effect
#' @export
showTypewriter <- function(filePath = NULL,
                           text = NULL,
                           speed = 5,
                           font = "vt323",
                           fontSize = 0.9,
                           color = "#00ff00",
                           bgColor = "#000") {
  
  # Generate unique ID based on timestamp with microseconds
  id <- paste0("typewriter-", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6")))
  
  # Determine text source
  if (!is.null(text)) {
    # Trim leading and trailing whitespace/newlines
    content <- sub("^[\\s\\n]+", "", text)
    content <- sub("[\\s\\n]+$", "", content)
  } else if (!is.null(filePath)) {
    if (!file.exists(filePath)) {
      stop(paste("File not found:", filePath))
    }
    content <- paste(readLines(filePath, warn = FALSE), collapse = "\n")
  } else {
    stop("Either filePath or text must be provided")
  }
  
  # Font selection and Google Fonts link
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
  
  # Build CSS style string
  # Use !important for local fonts to override revealjs CSS
  font_decl <- if (!is.null(fontConfig$local)) {
    sprintf("font-family: %s !important", fontConfig$family)
  } else {
    sprintf("font-family: %s", fontConfig$family)
  }
  
  style <- sprintf(
    "%s; white-space: pre; font-size: %sem; color: %s; background-color: %s; padding: 20px; border-radius: 5px; width: 800px; margin: 0 auto;",
    font_decl, fontSize, color, bgColor
  )
  
  # Escape for JavaScript (order matters!)
  content_escaped <- gsub("\\\\", "\\\\\\\\", content)  # Escape backslashes first
  content_escaped <- gsub("\n", "\\\\n", content_escaped)  # Escape newlines
  content_escaped <- gsub("\r", "\\\\r", content_escaped)  # Escape carriage returns
  content_escaped <- gsub("`", "\\\\`", content_escaped)   # Escape backticks
  content_escaped <- gsub("\\$", "\\\\$", content_escaped) # Escape dollar signs
  
  # Generate HTML with font preload or local @font-face
  if (!is.null(fontConfig$link)) {
    # Google Fonts
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
  cat(sprintf('  const text = `%s`;\n', content_escaped))
  cat(sprintf('  const containerId = "%s";\n', id))
  cat(sprintf('  const speed = %d;\n\n', speed))
  cat('  // Per-container registry prevents double init/overlapping timers\n')
  cat('  window.__typewriterRegistry = window.__typewriterRegistry || {};\n')
  cat('  if (!window.__typewriterRegistry[containerId]) {\n')
  cat('    window.__typewriterRegistry[containerId] = { running: false, timeoutId: null };\n')
  cat('  }\n\n')
  cat('  let i = 0;\n\n')
  cat('  function typeChar() {\n')
  cat('    const container = document.getElementById(containerId);\n')
  cat('    if (!container) { window.__typewriterRegistry[containerId].running = false; return; }\n')
  cat('    if (i < text.length) {\n')
  cat('      container.textContent += text.charAt(i);\n')
  cat('      i++;\n')
  cat('      window.__typewriterRegistry[containerId].timeoutId = setTimeout(typeChar, speed);\n')
  cat('    } else {\n')
  cat('      window.__typewriterRegistry[containerId].running = false;\n')
  cat('      window.__typewriterRegistry[containerId].timeoutId = null;\n')
  cat('    }\n')
  cat('  }\n\n')
  cat('  function initTypewriter() {\n')
  cat('    const container = document.getElementById(containerId);\n')
  cat('    if (!container) return;\n')
  cat('    const slide = container.closest(".slide");\n')
  cat('    if (!slide || !slide.classList.contains("present")) return;\n')
  cat('    const state = window.__typewriterRegistry[containerId];\n')
  cat('    if (state.running) return;\n')
  cat('    i = 0;\n')
  cat('    container.textContent = "";\n')
  cat('    if (state.timeoutId) clearTimeout(state.timeoutId);\n')
  cat('    state.running = true;\n')
  cat('    state.timeoutId = setTimeout(typeChar, 300);\n')
  cat('  }\n\n')
  cat('  function setup() {\n')
  cat('    if (typeof Reveal !== "undefined") {\n')
  cat('      Reveal.on("slidechanged", initTypewriter);\n')
  cat('      Reveal.on("ready", initTypewriter);\n')
  cat('      setTimeout(initTypewriter, 0);\n')
  cat('    } else {\n')
  cat('      initTypewriter();\n')
  cat('    }\n')
  cat('  }\n\n')
  cat('  if (document.readyState === "complete") {\n')
  cat('    setup();\n')
  cat('  } else {\n')
  cat('    window.addEventListener("load", setup);\n')
  cat('  }\n')
  cat('})();\n')
  cat('</script>\n\n')
  cat(sprintf('<style>\n#%s { %s }\n</style>\n\n', id, style))
  invisible(NULL)
}
