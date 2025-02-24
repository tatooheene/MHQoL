#' Provides the scores of the MHQoL based on the textual input (as described in the manual)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function provides the scores of the MHQoL based on textual input as described in the manual of the MHQoL.
#'
#' @aliases mhqol_states_to_scores
#'
#' @usage mhqol_states_to_scores(
#'   states,
#'   ignore_invalid = FALSE,
#'   ignore_NA = FALSE,
#'   retain_old_variables = TRUE)
#'
#' @param states A dataframe, character vector or list containing the states of the MHQoL.
#' Must contain the following states: SI (Self-Image), IN (INdependence),
#' MO (MOod), RE (RElationships), DA (Daily Activities), PH (Physical Health), FU (FUture).
#'
#' @param ignore_invalid If TRUE, the function will ignore missing states and continue processing.
#' If FALSE, the function will stop and throw an error.
#'
#' @param ignore_NA If TRUE, the function will ignore NA values in the input data.
#' If FALSE, the function will stop and throw an error.
#'
#' @param retain_old_variables If TRUE, the function will return the original states along with the new scores.
#' If FALSE, the function will only return the new scores.
#'
#' @return A dataframe containing the new scores based on the MHQoL manual.
#'
#' @keywords MHQoL
#' @keywords Scores
#' @keywords States
#'
#' @examples
#' # Example usage of the mhqol_scores function
#' # Get the MHQoL scores based on a character vector and do not retain old values
#' mhqol_scores(
#'   states = c(
#'     SI = "I think very positively about myself",
#'     IN = "I am very satisfied with my level of independence",
#'     MO = "I do not feel anxious, gloomy, or depressed",
#'     RE = "I am very satisfied with my relationships",
#'     DA = "I am very satisfied with my daily activities",
#'     PH = "I have no physical health problems",
#'     FU = "I am very optimistic about my future"
#'     ),
#'   retain_old_variables = FALSE
#' )
#'
#' # Get the MHQoL scores based on a DataFrame and retain old values
#' mhqol_scores(states = df)
#'
#' # Get the MHQoL scores based on a DataFrame and ignore missing states
#' mhqol_scores(states = df, ignore_invalid = TRUE)


mhqol_states_to_scores <- function(states,
                         ignore_invalid = FALSE,
                         ignore_NA = FALSE,
                         retain_old_variables = TRUE) {

  # Convert the different input types into a dataframe
  convert_to_df <- function(states){
    # If input is a dataframe
    if(is.data.frame(states)){
      return(states)
    }
    # If input is a character vector
    else if(is.character(states)){
      if(is.null(names(states))){
        stop("Character vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(states), nrow = 1))
      names(df) <- names(states)
      for(dim in names(states)){
        df[[dim]] <- states[dim]
      }

      return(df)
    }
    # If input is a numeric vector
    else if(is.numeric(states)){
      if(is.null(names(states))){
        stop("Numeric vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(states), nrow = 1))
      names(df) <- names(states)
      for(dim in names(states)){
        df[[dim]] <- states[dim]
      }

      return(df)
    }
    # If input is a list
    else if(is.list(states)){
      if(is.null(names(states))){
        stop("List must have names for dimension mapping")
      }
      return(as.data.frame(states, stringsAsFactors = FALSE))
    } else{
      stop("Invalid input type. Please provide a dataframe, character vector or list")
    }
  }

  states <- convert_to_df(states)

# Required states
required_states <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")



# Check for missing states
missing_states <- setdiff(required_states, colnames(states))



if(length(missing_states) > 0){
  if(ignore_invalid == FALSE){
  stop(paste(
    "The following required states are missing:",
    paste(missing_states, collapse = ",")
  ))
} else if(ignore_invalid == TRUE){
  warning(paste(
    "The following required states are missing and will be ignored:",
    paste(missing_states, collapse = ",")
  ))
}
}

  # Keep only valid columns in the data frame (instead of overwriting 'states' with a character vector)
  states <- states[, setdiff(colnames(states), missing_states), drop = FALSE]




if(any(is.na(states))){
  if(ignore_NA == FALSE){
    stop("The data contains NA values. Please handle NAs or set ignore_NA = TRUE.")
  } else if (ignore_NA == TRUE){
    warning("The data contains NA values. They willl be ignored in processing")
    }
}

if(all(sapply(states, is.numeric))){
  new_states <- states |>
    dplyr::mutate(
      SI_s = if("SI" %in% colnames(states)){
        dplyr::case_when(SI == 3 ~  3, # SELF-IMAGE
                         SI == 2  ~ 2,
                         SI == 1 ~ 1,
                         SI == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      IN_s = if("IN" %in% colnames(states)){
        dplyr::case_when(IN == 3 ~ 3, # INDEPENDENCE
                         IN == 2 ~ 2,
                         IN == 1 ~ 1,
                         IN == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      MO_s = if("MO" %in% colnames(states)){

        dplyr::case_when(MO == 3 ~ 3,    # MOOD
                         MO == 2 ~ 2,
                         MO == 1 ~ 1,
                         MO == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      RE_s = if("RE" %in% colnames(states)){
        dplyr::case_when(RE == 3 ~ 3,     # RELATIONSHIPS
                         RE == 2 ~ 2,
                         RE == 1 ~ 1,
                         RE == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      DA_s = if("DA" %in% colnames(states)){
        dplyr::case_when(DA == 3 ~ 3,  # DAILY ACTIVITIES
                         DA == 2 ~ 2,
                         DA == 1 ~ 1,
                         DA == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      PH_s = if("PH" %in% colnames(states)){
        dplyr::case_when(PH == 3 ~ 3,  # PHYSICAL HEALTH
                         PH == 2 ~ 2,
                         PH == 1 ~ 1,
                         PH == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      },
      FU_s = if("FU" %in% colnames(states)){
        dplyr::case_when(FU == 3 ~ 3, # FUTURE
                         FU == 2 ~ 2,
                         FU == 1 ~ 1,
                         FU == 0 ~ 0,
                         TRUE ~ NA_real_)
      }else{
        NA_real_
      }
    )


}else if(all(sapply(states, is.character))){

  # Define valid response mappings
  valid_states <- list(
    SI = c("I think very positively about myself",
           "I think positively about myself",
           "I think negatively about myself",
           "I think very negatively about myself"),

    IN = c("I am very satisfied with my level of independence",
           "I am satisfied with my level of independence",
           "I am dissatisfied with my level of independence",
           "I am very dissatisfied with my level of independence"),

    MO = c("I do not feel anxious, gloomy, or depressed",
           "I feel a little anxious, gloomy, or depressed",
           "I feel anxious, gloomy, or depressed",
           "I feel very anxious, gloomy, or depressed"),

    RE = c("I am very satisfied with my relationships",
           "I am satisfied with my relationships",
           "I am dissatisfied with my relationships",
           "I am very dissatisfied with my relationships"),

    DA = c("I am very satisfied with my daily activities",
           "I am satisfied with my daily activities",
           "I am dissatisfied with my daily activities",
           "I am very dissatisfied with my daily activities"),

    PH = c("I have no physical health problems",
           "I have some physical health problems",
           "I have many physical health problems",
           "I have a great many physical health problems"),

    FU = c("I am very optimistic about my future",
           "I am optimistic about my future",
           "I am gloomy about my future",
           "I am very gloomy about my future")
  )

  # Function to validate state responses
  validate_states <- function(df, valid_states) {
    for (col in names(valid_states)) {
      if (col %in% colnames(df)) {
        invalid_values <- unique(df[[col]][!df[[col]] %in% valid_states[[col]] & !is.na(df[[col]])])
        if (length(invalid_values) > 0) {
          stop(paste("Error: Column", col, "contains unexpected values:", paste(invalid_values, collapse = ", ")))
        }
      }
    }
  }

  # Run validation first
  validate_states(states, valid_states)


new_states <- states |>
      dplyr::mutate(
        SI_s = if("SI" %in% colnames(states)){
             dplyr::case_when(SI == "I think very positively about myself"~  3, # SELF-IMAGE
                              SI == "I think positively about myself"  ~ 2,
                              SI == "I think negatively about myself" ~ 1,
                              SI == "I think very negatively about myself" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        IN_s = if("IN" %in% colnames(states)){
             dplyr::case_when(IN == "I am very satisfied with my level of independence" ~ 3, # INDEPENDENCE
                              IN == "I am satisfied with my level of independence" ~ 2,
                              IN == "I am dissatisfied with my level of independence" ~ 1,
                              IN == "I am very dissatisfied with my level of independence" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        MO_s = if("MO" %in% colnames(states)){

             dplyr::case_when(MO == "I do not feel anxious, gloomy, or depressed" ~ 3,    # MOOD
                              MO == "I feel a little anxious, gloomy, or depressed" ~ 2,
                              MO == "I feel anxious, gloomy, or depressed" ~ 1,
                              MO == "I feel very anxious, gloomy, or depressed" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        RE_s = if("RE" %in% colnames(states)){
             dplyr::case_when(RE == "I am very satisfied with my relationships" ~ 3,     # RELATIONSHIPS
                              RE == "I am satisfied with my relationships" ~ 2,
                              RE == "I am dissatisfied with my relationships" ~ 1,
                              RE == "I am very dissatisfied with my relationships" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        DA_s = if("DA" %in% colnames(states)){
             dplyr::case_when(DA == "I am very satisfied with my daily activities" ~ 3,  # DAILY ACTIVITIES
                              DA == "I am satisfied with my daily activities" ~ 2,
                              DA == "I am dissatisfied with my daily activities" ~ 1,
                              DA == "I am very dissatisfied with my daily activities" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        PH_s = if("PH" %in% colnames(states)){
             dplyr::case_when(PH == "I have no physical health problems" ~ 3,  # PHYSICAL HEALTH
                              PH == "I have some physical health problems" ~ 2,
                              PH == "I have many physical health problems" ~ 1,
                              PH == "I have a great many physical health problems" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        },
        FU_s = if("FU" %in% colnames(states)){
             dplyr::case_when(FU == "I am very optimistic about my future" ~ 3, # FUTURE
                              FU == "I am optimistic about my future" ~ 2,
                              FU == "I am gloomy about my future" ~ 1,
                              FU == "I am very gloomy about my future" ~ 0,
                              TRUE ~ NA_real_)
        }else{
            NA_real_
        }
      )

}

  if(retain_old_variables == FALSE){
    new_states <- new_states |>
      dplyr::select(dplyr::ends_with("_s"))

    return(new_states)
  }else if(retain_old_variables == TRUE){
    return(new_states)
  }
}
