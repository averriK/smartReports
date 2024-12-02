# nolint start
#' Build Quarto Report
#'
#' This function builds the Quarto report, handling pre-render and post-render tasks.
#' @param home_dir Path to the home directory.
#' @param build_dir Name of the yml folder.
#' @param index_filename Name of the index file.
#' @param quarto_filename Name of the Quarto configuration file.
#' @param language Language code ('EN', 'ES', etc.).
#' @param output_format Output formats (e.g., 'html', 'docx').
#' @param extensions Extensions to be removed.
#' @param postRender Logical. Should post-render be performed?
#' 
#' @importFrom utils tail
#' @import data.table
#' @import yaml 
#' 
#' @export
#' 
buildReport <- function(
    home_dir = ".",
    build_dir = "_build",
    index_filename = "index.qmd",
    quarto_filename = "_quarto.yml",
    language = "EN",
    output_format = c("html", "docx"),
    extensions = c("spl", "bst", "cls", "md", "aux", "log", "tex", "jpg", "sty","docx", "pdf", "html"),
    postRender = TRUE
    
) {
  # You can set global variables to NULL to avoid R CMD check warnings
  . <- NULL
  
  # Get package resources path
  extdata_path <- system.file("extdata", package = "buildReport")
  
  if (!dir.exists(build_dir)) {
    
    # Create and copy _yml folder
    
    dir.create(build_dir, recursive = TRUE)
    file.copy(
      from = list.files(extdata_path, full.names = TRUE),
      to = build_dir,
      recursive = TRUE,
      overwrite = TRUE
    )
    message("Project structure initialized successfully!")
  }
  
  
  
  # Define internal functions that utilize variables from render()
  .
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
        'output-dir' = build_dir
      ),
      engine = "knitr",
      jupyter = "python3"
    )
    DATA <- .merge(DATA, FIELD)

    # Bibliography
    PATH <- home_dir
    FILE <- list.files(file.path(PATH), pattern = "references\\.bib$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- list(bibliography = FILE[1])
      DATA <- .merge(DATA, FIELD)
    }

    # Authors
    FILE <- list.files(file.path(PATH), pattern = "_authors\\.yml$", recursive = TRUE, full.names = TRUE)
    has_authors_yml <- length(FILE) > 0
    if (has_authors_yml) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      FIELD$author <- Filter(.validAuthor, FIELD$author)
      DATA <- .merge(DATA, FIELD)
    }

    # Parameters
    FILE <- list.files(file.path(PATH), pattern = "_params\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      if (has_authors_yml) {
        FIELD$author <- NULL  # Remove author field if _authors.yml exists
      }
      DATA <- .merge(DATA, FIELD)
    }

    # Format-specific settings
    FILE <- list.files(file.path(PATH), pattern = "_format\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- read_yaml(FILE[1], readLines.warn = FALSE)
      FIELD$format <- FIELD$format[names(FIELD$format) %in% output_format]
      if ("els-pdf" %in% output_format && is.null(FIELD$format$`els-pdf`$journal$name)) {
        FIELD$format$`els-pdf`$journal$name <- gsub(DATA$subtitle, pattern = "\n", replacement = "")
      }
      DATA <- .merge(DATA, FIELD)
    }

    # Styles for DOCX
    FILE <- list.files(file.path(PATH), pattern = "styles\\.docx$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0 && "docx" %in% output_format) {
      FIELD <- list(format = list(docx = list('reference-doc' = FILE[1])))
      DATA <- .merge(DATA, FIELD)
    }

    # Styles for HTML
    FILE <- list.files(file.path(PATH), pattern = "styles\\.css$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0 && "html" %in% output_format) {
      FIELD <- list(format = list(html = list(css = FILE[1])))
      DATA <- .merge(DATA, FIELD)
    }

    # ---------------------------------------------------------------------------
    # LANGUAGE-DEPENDENT STAGE

    # Cross-references
    FILE <- list.files(file.path(PATH), pattern = "_crossref\\.yml$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      FIELD <- yaml::read_yaml(FILE[1], readLines.warn = FALSE)
      LDATA <- .merge(LDATA, FIELD[[LANG]])
    }

    # Title
    FILE <- list.files(PATH, pattern = "_TITLE\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      VAR <- brio::read_lines(FILE[1]) |> paste(collapse = "\n")
      FIELD <- list(title = VAR)
      LDATA <- .merge(LDATA, FIELD)
    }

    # Subtitle
    FILE <- list.files(PATH, pattern = "_SUBTITLE\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(FILE) > 0) {
      VAR <- brio::read_lines(FILE[1]) |> paste(collapse = "\n")
      FIELD <- list(subtitle = VAR)
      LDATA <- .merge(LDATA, FIELD)
    }

    # Abstract
    FILE <- list.files(PATH, pattern = "_ABSTRACT\\.qmd$", recursive = TRUE, full.names = TRUE)
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
    YAML_LIST <- .merge(DATA, LDATA)

    # Convert to YAML
    YAML <- yaml::as.yaml(YAML_LIST)
    YAML <- gsub(pattern = ":\\s*yes($|\\n)", replacement = ": true\\1", YAML)
    YAML <- gsub(pattern = ":\\s*no($|\\n)", replacement = ": false\\1", YAML)

    # Write to file
    FILE <- file.path(home_dir, quarto_filename)
    brio::write_lines(text = YAML, path = FILE)
  }
  
  # Post-render function
  .postRender <- function() {
    # Remove files with specified extensions in the project folder
    pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
    files <- list.files(
      path = home_dir,
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
  
  # Main execution flow
  tryCatch({
    .preRender()
    
    # Build the YAML configuration if _quarto.yml does not exist
    if (!file.exists(file.path(home_dir, quarto_filename))) {
      .buildYAML()
    }
    
    # Render the Quarto document
    # browser()
    quarto::quarto_render(input = file.path(home_dir, index_filename))
    
    # Call postRender() function
    if(postRender) .postRender()
    
  }, error = function(e) {
    message("Error during rendering: ", e$message)
    # Call postRender() even if there's an error
    if(postRender) .postRender()
    stop(e)
  })
}
# nolint end
