# nolint start
#' Build Quarto Report
#'
#' This function builds the Quarto report, handling pre-render and post-render tasks.
#' @param build_dir Name of the yml folder.
#' @param publish_dir Name of the publish folder.
#' @param index_filename Name of the index file.
#' @param quarto_filename Name of the Quarto configuration file.
#' @param language Language code ('EN', 'ES', etc.).
#' @param output_format Output formats (e.g., 'html', 'docx').
#' @param extensions Extensions to be removed.
#' @param render Logical. Should the Quarto document be rendered?
#' @importFrom utils tail
#' @import data.table
#' @import yaml 
#' 
#' @export
#' 
buildReport <- function(
    build_dir = "_build",
    publish_dir = "_publish",
    index_filename = "index.qmd",
    quarto_filename = "_quarto.yml",
    language = "EN",
    output_format = c("html"),
    extensions = c("spl", "bst", "cls", "md", "aux", "log", "tex", "jpg", "sty","docx", "pdf", "html"),
    render = TRUE
) {
  . <- NULL
  
  
  
  # Pre-render function
  .preRender <- function() {
    paths <- c(
      file.path(build_dir)
    )
    for (path in paths) {
      if (dir.exists(path)) {
        subdirs <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)[-1]
        unlink(subdirs, recursive = TRUE, force = TRUE)
      }
    }
  }
  
  # Build YAML function
  .buildYAML <- function(){
    
    LANG <- language
    
    # Initialize DATA and LDATA as empty lists
    DATA <- list()
    LDATA <- list()
    
    # ---------------------------------------------------------------------------
    # Language-independent stage
    
    # Project settings
    FIELD <- list(
      project = list(
        type = "default",
        'output-dir' = publish_dir
        
      ),
      engine = "knitr",
      jupyter = "python3"
    )
    DATA <- .merge(DATA, FIELD)
    
    # Bibliography
    FILE <- list.files(".", pattern = "references\\.bib$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- list(bibliography = FILE[1])
      DATA <- .merge(DATA, FIELD)
    }
    
    # Authors
    FILE <- list.files(".", pattern = "_authors\\.yml$", recursive = TRUE, full.names = TRUE)
    has_authors_yml <- length(FILE) > 0
    if (has_authors_yml) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      FIELD$author <- Filter(.validAuthor, FIELD$author)
      DATA <- .merge(DATA, FIELD)
    }
    
    # Parameters
    FILE <- list.files(".", pattern = "_params\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      if (has_authors_yml) {
        FIELD$author <- NULL  # Remove author field if _authors.yml exists
      }
      DATA <- .merge(DATA, FIELD)
    }
    
    # Format-specific settings
    FILE <- list.files(".", pattern = "_format\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      FIELD$format <- FIELD$format[names(FIELD$format) %in% output_format]
      if ("els-pdf" %in% output_format && is.null(FIELD$format$`els-pdf`$journal$name)) {
        FIELD$format$`els-pdf`$journal$name <- gsub(DATA$subtitle, pattern = "\n", replacement = "")
      }
      DATA <- .merge(DATA, FIELD)
    }
    
    # Styles for DOCX
    FILE <- list.files(".", pattern = "styles\\.docx$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0 && "docx" %in% output_format) {
      FIELD <- list(format = list(docx = list('reference-doc' = FILE[1])))
      DATA <- .merge(DATA, FIELD)
    }
    
    # Styles for HTML
    FILE <- list.files(".", pattern = "styles\\.css$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0 && "html" %in% output_format) {
      FIELD <- list(format = list(html = list(css = FILE[1])))
      DATA <- .merge(DATA, FIELD)
    }
    
    # ---------------------------------------------------------------------------
    # LANGUAGE-DEPENDENT STAGE
    
    # Cross-references
    FILE <- list.files(".", pattern = "_crossref\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- yaml::read_yaml(FILE[1], readLines.warn = FALSE)
      LDATA <- .merge(LDATA, FIELD[[LANG]])
    }
    
    # Title
    FILE <- list.files(".", pattern = "_TITLE\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      VAR <- brio::read_lines(FILE[1]) |> paste(collapse = "\n")
      FIELD <- list(title = VAR)
      LDATA <- .merge(LDATA, FIELD)
    }
    
    # Subtitle
    FILE <- list.files(".", pattern = "_SUBTITLE\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      VAR <- brio::read_lines(FILE[1]) |> paste(collapse = "\n")
      FIELD <- list(subtitle = VAR)
      LDATA <- .merge(LDATA, FIELD)
    }
    
    # Abstract
    FILE <- list.files(".", pattern = "_ABSTRACT\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      VAR <- brio::read_lines(FILE[1]) |> paste(collapse = "\n")
      FIELD <- list(abstract = VAR)
      LDATA <- .merge(LDATA, FIELD)
    }
    
    # Additional parameters
    FIELD <- list(params = list(
      background = "white",
      render = "none",
      ext = "png",
      lang = LANG
    ))
    LDATA <- .merge(LDATA, FIELD)
    
    # ---------------------------------------------------------------------------
    # Combine DATA and LDATA
    DATA <- .merge(DATA, LDATA)
    
    # Filter formats
    if (!is.null(DATA$format)) {
      DATA$format <- DATA$format[names(DATA$format) %in% output_format]
    }
    
    # Convert to YAML
    YAML <- yaml::as.yaml(DATA)
    YAML <- gsub(pattern = ":\\s*yes($|\\n)", replacement = ": true\\1", YAML)
    YAML <- gsub(pattern = ":\\s*no($|\\n)", replacement = ": false\\1", YAML)
    
    # Always remove existing quarto_filename, then recreate
    unlink(quarto_filename, recursive = FALSE, force = TRUE)
    brio::write_lines(text = YAML, path = quarto_filename)
  }
  
  # Post-render function
  .postRender <- function() {
    # Remove files with specified extensions in the project folder
    pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
    files <- list.files(
      path = ".",
      pattern = pattern,
      ignore.case = TRUE,
      full.names = TRUE
    )
    if (length(files) > 0) {
      file.remove(files)
    }
    
  }
  
  # Additional helper functions
  .merge <- function(x, y) {
    if (is.list(x) && is.list(y)) {
      for (name in names(y)) {
        if (name %in% names(x)) {
          x[[name]] <- .merge(x[[name]], y[[name]])
        } else {
          x[[name]] <- y[[name]]
        }
      }
      return(x)
    } else {
      return(y)
    }
  }
  
  .validAuthor <- function(author) {
    fields_to_check <- c("name", "orcid", "email")
    for (field in fields_to_check) {
      if (!is.null(author[[field]])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  # Get package resources path
  extdata_path <- system.file("extdata", package = "smartReports")

  # 1) Copy _extensions folder (if it exists) from extdata_path to current dir (".")
  extensions_folder <- file.path(extdata_path, "_extensions")
  if (dir.exists(extensions_folder)) {
    file.copy(
      from = extensions_folder,
      to = ".",   # copy to top-level working directory
      recursive = TRUE,
      overwrite = TRUE
    )
    message("Copied _extensions folder to current directory!")
  }

  # 2) Now copy all other support files/folders into build_dir
  if (!dir.exists(build_dir)) {
    dir.create(build_dir, recursive = TRUE)

    # Exclude the _extensions folder (already handled above)
    files_to_copy <- list.files(extdata_path, full.names = TRUE)
    files_to_copy <- files_to_copy[basename(files_to_copy) != "_extensions"]

    # Also exclude build_dir itself just in case
    excluded_folder <- basename(build_dir)
    files_to_copy <- files_to_copy[basename(files_to_copy) != excluded_folder]

    file.copy(
      from = files_to_copy,
      to = build_dir,
      recursive = TRUE,
      overwrite = TRUE
    )
    message("Project structure initialized successfully in build_dir!")
  }

  # Always refresh the publish_dir so it's guaranteed to be clean
  unlink(publish_dir, recursive = TRUE, force = TRUE)
  dir.create(publish_dir, recursive = TRUE)
  
  # Main execution flow
  tryCatch({
    .preRender()
    
    # Build the YAML configuration
    .buildYAML()

    if (render) {
      quarto::quarto_render(
        input = index_filename,
        output_format = output_format
      )
    }

    .postRender()
    
  }, error = function(e) {
    message("Error during rendering: ", e$message)
    .postRender()
    stop(e)
  })
}
# nolint end
