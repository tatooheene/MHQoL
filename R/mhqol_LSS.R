#' A function to calculate the Level Sum Scores (LSS) of the MHQoL
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function calculates the Level Sum Scores (LSS) per dimension of overall
#' of the MHQoL based on the scores of the different dimensions.
#'
#' @aliases mhqol_LSS
#'
#' @usage mhqol_LSS(
#'   dimensions,
#'   metric = c("average", "total"),
#'   ignore_invalid = FALSE,
#'   ignore_NA = TRUE)
#'
#' @param dimensions A dataframe, character vector or list containing the dimensions of the MHQoL.
#' Must contain the following dimensions: SI (Self-Image), IN (INdependence),
#' MO (MOod), RE (RElationships), DA (Daily Activities), PH (Physical Health), FU (FUture).
#'
#' @param metric A character value indicating whether to calculate the "total" or "average" LSS.
#'
#' @param ignore_invalid If TRUE, the function will ignore missing dimensions and continue processing.
#' If FALSE, the function will stop and throw an error.
#'
#' @param ignore_NA If TRUE, the function will ignore NA values in the input data.
#' If FALSE, the function will stop and throw an error.
#'
#' @return A dataframe containing the LSS based on the MHQoL manual.
#'
#' @keywords MHQoL
#' @keywords LSS
#' @keywords Dimensions
#'
#' @examples
#' # Example usage of the mhqol_LSS function
#'
#' # Get the LSS based on a character vector and calculate the total LSS,
#' # not all dimensions are present
#' mhqol_LSS(
#'   dimensions = c(IN = 2, MO = 3, RE = 2, DA = 1, PH = 2, FU = 3),
#'   metric = "total", ignore_invalid = TRUE)
#'
#' # Get the LSS based on a dataframe and calculate the average LSS,
#' # all dimensions are present
#' mhqol_LSS(
#'   dimensions =
#'   data.frame(SI = 1, IN = 2, MO = 3, RE = 2, DA = 1, PH = 2, FU = 3),
#'   metric = "average")

mhqol_LSS <- function(dimensions,
                      metric = c("average", "total"),
                      ignore_invalid = FALSE,
                      ignore_NA = TRUE) {

  # Ensure metric is a single valid value
  metric <- match.arg(metric)

  # Convert character dimensions to numeric scores
  if (all(sapply(dimensions, is.character))) {
    data <- mhqol::mhqol_states_to_scores(states = dimensions,
                                          ignore_invalid = ignore_invalid,
                                          ignore_NA = ignore_NA,
                                          retain_old_variables = FALSE)
  } else if (all(sapply(dimensions, is.numeric))) {
    data <- as.data.frame(dimensions)  # Ensure it's a dataframe

    validate_dimensions <- function(data) {
      # Define the columns to check
      columns_to_check <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")

      # Loop through each column and validate the values
      for (col in columns_to_check) {
        if (col %in% colnames(data)) {
          invalid_values <- data[[col]][!is.na(data[[col]]) & (data[[col]] < 0 | data[[col]] > 3)]

          if (length(invalid_values) > 0) {
            stop(paste("Error: Column", col, "contains values outside the range [0, 3]:", paste(invalid_values, collapse = ", ")))
          }
        }
      }
    }

    validate_dimensions(data)

  } else {
    stop("The 'dimensions' argument must be either all numeric or all character values.")
  }

  # Compute LSS scores
  data <- data |>
    dplyr::mutate(
      LSS = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)
    )

  # If metric is "total", return the dataframe
  if (metric == "total") {
    return(data)
  }

  # If metric is "average", compute mean scores for each dimension
  average_scores <- data |>
    dplyr::summarize(dplyr::across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

  return(average_scores)
}
