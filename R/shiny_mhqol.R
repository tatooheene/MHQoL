#' Launch Shiny MHQoL Interface
#'
#' \code{shiny_mhqol} launches a Shiny interface for browser-based MHQoL calculations.
#'
#' This function starts the Shiny app for Mental Health Quality of Life (MHQoL) scoring,
#' allowing users to interactively input data and calculate utility scores.
#'
#' @param display.mode The display mode to be passed to \link[shiny]{runApp}. Default is "normal".
#' @return NULL (launches the Shiny app).
#' @examples
#' if(interactive()) {
#'   shiny_mhqol()
#'   shiny_mhqol(display.mode = "normal")
#' }
#' @export


shiny_mhqol <- function(display.mode = "normal") {
  if (interactive()) {
    # List of required packages
    pkgs <- c(
      "shiny", "tidyverse", "DT", "writexl", "MHQoL",
      "fmsb", "here", "shinyalert"
    )

    # Check for missing packages
    missing <- sapply(pkgs, function(x) !requireNamespace(x, quietly = TRUE))

    # Stop execution if any required packages are missing
    if (any(missing)) {
      stop(
        paste(
          "The following package(s) are required for shiny_mhqol to work:",
          paste(pkgs[missing], collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Locate and run the Shiny app
    app_dir <- system.file("shiny", package = "mhqol")
    if (app_dir == "") {
      stop("Shiny app directory not found in the 'mhqol' package.", call. = FALSE)
    }

    shiny::runApp(app_dir, display.mode = display.mode)
  } else {
    stop("shiny_mhqol() should only be run in an interactive session.", call. = FALSE)
  }
}
