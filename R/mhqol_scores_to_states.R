#' Provides the states of the MHQoL based on the scores provided (as described in the manual)
#'
#' @description
#' This function provides the states of the MHQoL based on the scores provided (as described in the manual).
#'
#' @aliases mhqol_scores_to_states
#'
#' @usage mhqol_scores_to_states(
#'   scores,
#'   ignore_invalid = FALSE,
#'   ignore_NA = FALSE,
#'   retain_old_variables = TRUE)
#'
#' @param scores A dataframe, numeric vector, or list containing the scores of the MHQoL.
#'
#' @param ignore_invalid If TRUE, the function will ignore missing scores and continue processing.
#'
#' @param ignore_NA If TRUE, the function will ignore NA values in the input data.
#'
#' @param retain_old_variables If TRUE, the function will retain the old variables in the output.
#'
#' @return A dataframe containing the states of the MHQoL based on the scores provided.
#'
#' @keywords MHQoL
#' @keywords States
#' @keywords Scores
#'
#' @examples
#' # Example usage of the mhqol_scores_to_states function
#' # Get the states based on a numeric vector, not all scores are present
#' mhqol_scores_to_states(
#'   scores = c(IN = 2, DA = 1, PH = 2, FU = 3),
#'   ignore_invalid = TRUE
#' )
#'
#' # Get the states based on a dataframe
#' mhqol_scores_to_states(
#'   scores = data.frame(
#'     SI = 1,
#'     IN = 2,
#'     MO = 3,
#'     RE = 2,
#'     DA = 1,
#'     PH = 2,
#'     FU = 3
#'     )
#'  )

mhqol_scores_to_states  <- function(scores,
                                   ignore_invalid = FALSE,
                                   ignore_NA = FALSE,
                                   retain_old_variables = TRUE){


  # Convert the different input types into a dataframe
  convert_to_df <- function(scores){
    # If input is a dataframe
    if(is.data.frame(scores)){
      return(scores)
    }

    # If input is a numeric vector
    else if(is.numeric(scores)){
      if(is.null(names(scores))){
        stop("Numeric vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(scores), nrow = 1))
      names(df) <- names(scores)
      for(dim in names(scores)){
        df[[dim]] <- scores[dim]
      }

      return(df)
    }
    # If input is a list
    else if(is.list(scores)){
      if(is.null(names(scores))){
        stop("List must have names for dimension mapping")
      }
      return(as.data.frame(scores, stringsAsFactors = FALSE))
    } else{
      stop("Invalid input type. Please provide a dataframe, numeric vector or list")
    }
  }

  scores <- convert_to_df(scores)

  # Required scores
  required_scores <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")



  # Check for missing scores
  missing_scores <- setdiff(required_scores, colnames(scores))



  if(length(missing_scores) > 0){
    if(ignore_invalid == FALSE){
      stop(paste(
        "The following required scores are missing:",
        paste(missing_scores, collapse = ",")
      ))
  } else if(ignore_invalid == TRUE){
    warning(paste(
      "The following required scores are missing and will be ignored:",
      paste(missing_scores, collapse = ",")
    ))


  }
  }

  # Remove missing scores from processing
scores <- scores[, setdiff(colnames(scores), missing_scores), drop = FALSE]

  if(any(is.na(scores))){
    if(ignore_NA == FALSE){
      stop("The data contains NA values. Please handle NAs or set ignore_NA = TRUE.")
    } else if (ignore_NA == TRUE){
      warning("The data contains NA values. They willl be ignored in processing")
    }
  }


  if(all(sapply(scores, is.numeric))){
    new_scores <- scores |>
      dplyr::mutate(
        SI_s = if("SI" %in% colnames(scores)){
          if(any(SI < 0 | SI > 3, na.rm = TRUE)) stop("Error: SI contains values outside [0,3]")
          dplyr::case_when(SI == 3 ~ "I think very positively about myself", # SELF-IMAGE
                           SI == 2 ~ "I think positively about myself",
                           SI == 1 ~ "I think negatively about myself",
                           SI == 0 ~ "I think very negatively about myself",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        IN_s = if("IN" %in% colnames(scores)){
          if(any(IN < 0 | IN > 3, na.rm = TRUE)) stop("Error: IN contains values outside [0,3]")
          dplyr::case_when(IN == 3 ~ "I am very satisfied with my level of independence", # INDEPENDENCE
                           IN == 2 ~ "I am satisfied with my level of independence",
                           IN == 1 ~ "I am dissatisfied with my level of independence",
                           IN == 0 ~ "I am very dissatisfied with my level of independence",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        MO_s = if("MO" %in% colnames(scores)){
          if(any(MO < 0 | MO > 3, na.rm = TRUE)) stop("Error: MO contains values outside [0,3]")
          dplyr::case_when(MO == 3 ~ "I do not feel anxious, gloomy, or depressed",    # MOOD
                           MO == 2 ~ "I feel a little anxious, gloomy, or depressed",
                           MO == 1 ~ "I feel anxious, gloomy, or depressed",
                           MO == 0 ~ "I feel very anxious, gloomy, or depressed",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        RE_s = if("RE" %in% colnames(scores)){
          if(any(RE < 0 | RE > 3, na.rm = TRUE)) stop("Error: RE contains values outside [0,3]")
          dplyr::case_when(RE == 3 ~ "I am very satisfied with my relationships",     # RELATIONSHIPS
                           RE == 2 ~ "I am satisfied with my relationships",
                           RE == 1 ~ "I am dissatisfied with my relationships",
                           RE == 0 ~  "I am very dissatisfied with my relationships",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        DA_s = if("DA" %in% colnames(scores)){
          if(any(DA < 0 | DA > 3, na.rm = TRUE)) stop("Error: DA contains values outside [0,3]")
          dplyr::case_when(DA == 3 ~ "I am very satisfied with my daily activities",  # DAILY ACTIVITIES
                           DA == 2 ~ "I am satisfied with my daily activities",
                           DA == 1 ~ "I am dissatisfied with my daily activities",
                           DA ==  0 ~ "I am very dissatisfied with my daily activities",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        PH_s = if("PH" %in% colnames(scores)){
          if(any(PH < 0 | PH > 3, na.rm = TRUE)) stop("Error: PH contains values outside [0,3]")
          dplyr::case_when(PH == 3 ~ "I have no physical health problems",  # PHYSICAL HEALTH
                           PH == 2 ~ "I have some physical health problems",
                           PH == 1 ~ "I have many physical health problems" ,
                           PH == 0 ~  "I have a great many physical health problems",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        FU_s = if("FU" %in% colnames(scores)){
          if(any(FU < 0 | FU > 3, na.rm = TRUE)) stop("Error: FU contains values outside [0,3]")
          dplyr::case_when(FU == 3 ~ "I am very optimistic about my future", # FUTURE
                           FU == 2 ~ "I am optimistic about my future",
                           FU == 1 ~ "I am gloomy about my future",
                           FU == 0 ~ "I am very gloomy about my future",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        }
      )

  } else{
    stop("All scores must be numeric")
  }

if(retain_old_variables == FALSE){
  new_scores <- new_scores |>
    dplyr::select(dplyr::ends_with("_s"))

  return(new_scores)
}else if(retain_old_variables == TRUE){
  return(new_scores)
}


}
