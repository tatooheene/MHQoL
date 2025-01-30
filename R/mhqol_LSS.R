#' A function to calculate the Level Sum Scores (LSS) of the MHQoL
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function calculates the Level Sum Scores (LSS) per dimension of overall
#' of the MHQoL based on the scores of the different dimensions.
#'
#' @param dimensions A dataframe, character vector or list containing the dimensions of the MHQoL.
#' Must contain the following dimensions: SI (Self-Image), IN (INdependence),
#' MO (MOod), RE (RElationships), DA (Daily Activities), PH (Physical Health), FU (FUture).
#'
#' @param metric A character value indicating whether to calculate the "total" or "average" LSS.
#'
#' @param ignore.invalid If TRUE, the function will ignore missing dimensions and continue processing.
#' If FALSE, the function will stop and throw an error.
#'
#' @param ignore.NA If TRUE, the function will ignore NA values in the input data.
#' If FALSE, the function will stop and throw an error.
#'
#' @return A dataframe containing the LSS based on the MHQoL manual.
#'
#' @keywords MHQoL, LSS, Dimensions
#'
#' @examples
#' # Example usage of the mhqol_LSS function
#' # Get the LSS based on a character vector and calculate the total LSS, not all dimensions are present
#' mhqol_LSS(dimensions = c(IN = 2, MO = 3, RE = 2, DA = 1, PH = 2, FU = 3), metric = "total", ignore.invalid = TRUE)
#'
#' # Get the LSS based on a dataframe and calculate the average LSS, all dimensions are present
#' mhqol_LSS(dimensions = data.frame(SI = 1, IN = 2, MO = 3, RE = 2, DA = 1, PH = 2, FU = 3), metric = "average")

#HIER NOG TATOOHEENE VOORZETTEN!
mhqol_LSS <- function(dimensions,
                      metric = c("average", "total"),
                      ignore.invalid = FALSE,
                      ignore.NA = TRUE) {


  # Check if metric is a single value
  if (length(metric) != 1) {
    stop("The 'metric' argument must be a single value. Please choose either 'total' or 'average' ")
  }

  # Check if metric is either "total" or "average"
  if (!metric %in% c("total", "average")) {
    stop("Invalid metric chosen. Please choose either 'total' or 'average'.")
  }

  # Check whether the input are characters or numeric

  if(all(sapply(dimensions, is.character))){
data <-  mhqol::mhqol_states_to_scores(dimensions = dimensions,
               ignore.invalid = ignore.invalid,
               ignore.NA = ignore.NA,
               retain_old_variables = FALSE)
return(data)

  }else if((all(sapply(dimensions, is.numeric)))){
  return(data)
}


    # If the chosen metric is "total", provide a total score per participant
    if(metric == "total"){
      data <- data |>
        dplyr::mutate(
          LSS = rowSums(dplyr::across(dplyr::everything()), na.rm = TRUE)
        )

    return(data)

    # If the chosen metric is "average", provide an average score per dimension and overall
    } else if(metric == "average"){

      data <- data |>
        dplyr::mutate(
          LSS = rowSums(dplyr::across(dplyr::everything()), na.rm = TRUE)
        )

      SI_LSS_average <- mean(data$SI_s, na.rm = TRUE)
      IN_LSS_average <- mean(data$IN_s, na.rm = TRUE)
      MO_LSS_average <- mean(data$MO_s, na.rm = TRUE)
      RE_LSS_average <- mean(data$RE_s, na.rm = TRUE)
      DA_LSS_average <- mean(data$DA_s, na.rm = TRUE)
      PH_LSS_average <- mean(data$PH_s, na.rm = TRUE)
      FU_LSS_average <- mean(data$FU_s, na.rm = TRUE)
      LSS_average <- mean(data$LSS, na.rm = TRUE)

      df <- data.frame(SI_LSS_average, IN_LSS_average, MO_LSS_average, RE_LSS_average, DA_LSS_average, PH_LSS_average, FU_LSS_average, LSS_average)

      return(df)
    }

  }







