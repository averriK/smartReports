#' Build and display typewriter index from chapter list
#'
#' @param index Character vector with chapter/section names
#' @param speed Speed in milliseconds per character (default 5)
#' @param font Font name (default "vt323")
#' @param fontSize Font size in em units (default 0.9)
#' @param color Text color in hex (default "#00ff00")
#' @param bgColor Background color in hex (default "#000")
#' @return Prints HTML with typewriter effect
#' @export
buildIndexTypewriter <- function(index, 
                                speed = 5, 
                                font = "vt323",
                                fontSize = 0.9,
                                color = "#00ff00",
                                bgColor = "#000") {
  
  if (length(index) == 0) {
    stop("index must contain at least one element")
  }
  
  # Build the index text
  lines <- c(">> MODULE INDEX:", "")
  
  for (i in seq_along(index)) {
    # Format number as 01, 02, ... 09, 10, ...
    num <- sprintf("%02d", i)
    # Format line
    line <- sprintf("   [%s] -- %s", num, index[i])
    lines <- c(lines, line)
  }
  
  # Add closing
  lines <- c(lines, "", ">> _")
  
  # Combine into single text
  text <- paste(lines, collapse = "\n")
  
  # Call showTypewriter with the generated text and parameters
  showTypewriter(text = text, 
                 speed = speed, 
                 font = font,
                 fontSize = fontSize,
                 color = color,
                 bgColor = bgColor)
}
