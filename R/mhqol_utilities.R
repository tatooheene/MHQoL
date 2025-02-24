#' Provides the scores of the MHQoL based on the textual input (as described in the manual)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function provides the utilities of the MHQoL based on
#' textual, as described in the manual, or numeric input of the MHQoL.
#'
#' @aliases mhqol_utilities
#'
#' @usage mhqol_utilities(
#'   dimensions,
#'   country = "Netherlands",
#'   ignore_invalid = FALSE,
#'   ignore_NA = TRUE,
#'   retain_old_variables = TRUE)
#'
#' @param dimensions A dataframe, character vector, numeric vector, or list containing
#' the character or numeric dimensions of the MHQoL.
#' Must contain the following dimensions: SI (Self-Image), IN (INdependence),
#' MO (MOod), RE (RElationships), DA (Daily Activities), PH (Physical Health), FU (FUture).
#'
#' @param country The country for which the utilities should be calculated. For now the only option is "Netherlands".
#'
#' @param ignore_invalid If TRUE, the function will ignore missing dimensions and continue processing.
#' If FALSE, the function will stop and throw an error.
#'
#' @param ignore_NA If TRUE, the function will ignore NA values in the input data.
#' If FALSE, the function will stop and throw an error.
#'
#' @param retain_old_variables If TRUE, the function will return the original dimensions along with the new utilities.
#' If FALSE, the function will only return the new utilities.
#'
#' @return A dataframe containing the new utilities based on the MHQoL manual.
#'
#' @keywords MHQoL
#' @keywords Utilities
#' @keywords Dimensions
#'
#' @examples
#' # Example usage of the mhqol_utilities function
#'
#' # Get the MHQoL utilities based on a character vector and do not retain old values
#' mhqol_utilities(
#'   dimensions = c(
#'     SI = "I think very positively about myself",
#'     IN = "I am very satisfied with my level of independence",
#'     MO = "I do not feel anxious, gloomy, or depressed",
#'     RE = "I am very satisfied with my relationships",
#'     DA = "I am very satisfied with my daily activities",
#'     PH = "I have no physical health problems",
#'     FU = "I am very optimistic about my future"
#'    ),
#'    retain_old_variables = FALSE
#' )
#'
#' # Get the MHQoL utilities based on a DataFrame and retain old values
#' mhqol_utilities(dimensions = df)
#'
#' # Get the MHQoL utilities based on a DataFrame and ignore missing dimensions
#' mhqol_utilities(dimensions = df, ignore_invalid = TRUE)
#'
#' # Get the MHQoL utilities based on a numeric vector and ignore missing dimensions
#' mhqol_utilities(
#'   dimensions = c(IN = 2, MO = 1, RE = 0, DA = 3, PH = 2, FU = 1),
#'   ignore_invalid = TRUE
#' )



mhqol_utilities <- function(dimensions,
                            country = "Netherlands",
                            ignore_invalid = FALSE,
                            ignore_NA = TRUE,
                            retain_old_variables = TRUE) {


  # Check if the country exists in the dataframe
  if (!country %in% names(df_utilities_countries)) {
    stop("The specified country is not in the dataset.")
  }

  # Convert the different input types into a dataframe
  convert_to_df <- function(dimensions){
    # If input is a dataframe
    if(is.data.frame(dimensions)){
      return(dimensions)
    }
    # If input is a character vector
    else if(is.character(dimensions)){
      if(is.null(names(dimensions))){
        stop("Character vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(dimensions), nrow = 1))
      names(df) <- names(dimensions)
      for(dim in names(dimensions)){
        df[[dim]] <- dimensions[dim]
      }

      return(df)
    }
    # If input is a numeric vector
    else if(is.numeric(dimensions)){
      if(is.null(names(dimensions))){
        stop("Numeric vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(dimensions), nrow = 1))
      names(df) <- names(dimensions)
      for(dim in names(dimensions)){
        df[[dim]] <- dimensions[dim]
      }

      return(df)
    }
    # If input is a list
    else if(is.list(dimensions)){
      if(is.null(names(dimensions))){
        stop("List must have names for dimension mapping")
      }
      return(as.data.frame(dimensions, stringsAsFactors = FALSE))
    } else{
      stop("Invalid input type. Please provide a dataframe, character vector, numeric vector or list")
    }
  }

  dimensions <- convert_to_df(dimensions)

  # Required dimensions
  required_dimensions <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")



  # Check for missing dimensions
  missing_dimensions <- setdiff(required_dimensions, colnames(dimensions))



  if(length(missing_dimensions) > 0){
    if(ignore_invalid == FALSE){
      stop(paste(
        "The following required dimensions are missing:",
        paste(missing_dimensions, collapse = ",")
      ))
  } else if(ignore_invalid == TRUE){
    warning(paste(
      "The following required dimensions are missing and will be ignored:",
      paste(missing_dimensions, collapse = ",")
    ))


  }
}

# Remove missing utilities from processing
dimensions <- dimensions[, setdiff(colnames(dimensions), missing_dimensions), drop = FALSE]



  if(any(is.na(dimensions))){
    if(ignore_NA == FALSE){
      stop("The data contains NA values. Please handle NAs or set ignore_NA = TRUE.")
    } else if (ignore_NA == TRUE){
      warning("The data contains NA values. They willl be ignored in processing")
    }
  }

  if(all(sapply(dimensions, is.numeric))){
    new_dimensions <- dimensions |>
      dplyr::mutate(
        SI_u = if("SI" %in% colnames(dimensions)){
          if(any(!SI %in% 0:3 & !is.na(SI))) stop("Error: SI contains values outside [0,3]")
        dplyr::case_when(SI == 3 ~  df_utilities_countries[df_utilities_countries$dimensions =="SI_3", country], # SELF-IMAGE
                                            SI == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_2", country],
                                            SI == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_1", country],
                                            SI == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_0", country],
                         TRUE ~ NA_real_)
        }else{
          NA_real_
        },
        IN_u = if("IN" %in% colnames(dimensions)){
          if(any(!IN %in% 0:3 & !is.na(IN))) stop("Error: IN contains values outside [0,3]")
                    dplyr::case_when(IN == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_3", country], # INDEPENDENCE
                                            IN == 2  ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_2", country],
                                            IN == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_1", country],
                                            IN == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        MO_u = if("MO" %in% colnames(dimensions)){
          if(any(!MO %in% 0:3 & !is.na(MO))) stop("Error: MO contains values outside [0,3]")
                    dplyr::case_when(MO == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_3", country],    # MOOD
                                            MO == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_2", country],
                                            MO == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_1", country],
                                            MO == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        RE_u = if("RE" %in% colnames(dimensions)){
          if(any(!RE %in% 0:3 & !is.na(RE))) stop("Error: RE contains values outside [0,3]")
                    RE_u = dplyr::case_when(RE == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_3", country],     # RELATIONSHIPS
                                            RE == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_2", country],
                                            RE == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_1", country],
                                            RE == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        DA_u = if("DA" %in% colnames(dimensions)){
          if(any(!DA %in% 0:3 & !is.na(DA))) stop("Error: DA contains values outside [0,3]")
                    DA_u = dplyr::case_when(DA == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_3", country],  # DAILY ACTIVITIES
                                            DA == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_2", country],
                                            DA == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_1", country],
                                            DA == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        },
        PH_u = if("PH" %in% colnames(dimensions)){
          if(any(!PH %in% 0:3 & !is.na(PH))) stop("Error: PH contains values outside [0,3]")
                    PH_u = dplyr::case_when(PH == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_3", country],  # PHYSICAL HEALTH
                                            PH == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_2", country],
                                            PH == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_1", country],
                                            PH == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        FU_u = if("FU" %in% colnames(dimensions)){
          if(any(!FU %in% 0:3 & !is.na(FU))) stop("Error: FU contains values outside [0,3]")
                    FU_u = dplyr::case_when(FU == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="FU_3", country], # FUTURE
                                            FU == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="FU_2", country],
                                            FU == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="FU_1", country],
                                            FU == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="FU_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        }
      )




  }else if(all(sapply(dimensions, is.character))){
    # Define valid response mappings
    valid_dimensions <- list(
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
    validate_dimensions <- function(df, valid_dimensions) {
      for (col in names(valid_dimensions)) {
        if (col %in% colnames(df)) {
          invalid_values <- unique(df[[col]][!df[[col]] %in% valid_dimensions[[col]] & !is.na(df[[col]])])
          if (length(invalid_values) > 0) {
            stop(paste("Error: Column", col, "contains unexpected values:", paste(invalid_values, collapse = ", ")))
          }
        }
      }
    }

    # Run validation first
    validate_dimensions(dimensions, valid_dimensions)


    new_dimensions <- dimensions |>
      dplyr::mutate(
        SI_u = if("SI" %in% colnames(dimensions)){
          dplyr::case_when(SI == "I think very positively about myself"~ df_utilities_countries[df_utilities_countries$dimensions == "SI_3", country],# SELF-IMAGE
                                            SI == "I think positively about myself"  ~ df_utilities_countries[df_utilities_countries$dimensions == "SI_2", country],
                                            SI == "I think negatively about myself" ~ df_utilities_countries[df_utilities_countries$dimensions == "SI_1", country],
                                            SI == "I think very negatively about myself" ~ df_utilities_countries[df_utilities_countries$dimensions == "SI_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        },
        IN_u = if("IN" %in% colnames(dimensions)){
                    dplyr::case_when(IN == "I am very satisfied with my level of independence" ~ df_utilities_countries[df_utilities_countries$dimensions == "IN_3", country],# INDEPENDENCE
                                            IN == "I am satisfied with my level of independence" ~ df_utilities_countries[df_utilities_countries$dimensions == "IN_2", country],
                                            IN == "I am dissatisfied with my level of independence" ~ df_utilities_countries[df_utilities_countries$dimensions == "IN_1", country],
                                            IN == "I am very dissatisfied with my level of independence" ~ df_utilities_countries[df_utilities_countries$dimensions == "IN_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        MO_u = if("MO" %in% colnames(dimensions)){
                    dplyr::case_when(MO == "I do not feel anxious, gloomy, or depressed" ~ df_utilities_countries[df_utilities_countries$dimensions == "MO_3", country],    # MOOD
                                            MO == "I feel a little anxious, gloomy, or depressed" ~ df_utilities_countries[df_utilities_countries$dimensions == "MO_2", country],
                                            MO == "I feel anxious, gloomy, or depressed" ~ df_utilities_countries[df_utilities_countries$dimensions == "MO_1", country],
                                            MO == "I feel very anxious, gloomy, or depressed" ~ df_utilities_countries[df_utilities_countries$dimensions == "MO_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        RE_u = if("RE" %in% colnames(dimensions)){
                    RE_u = dplyr::case_when(RE == "I am very satisfied with my relationships" ~ df_utilities_countries[df_utilities_countries$dimensions == "RE_3", country],     # RELATIONSHIPS
                                            RE == "I am satisfied with my relationships" ~ df_utilities_countries[df_utilities_countries$dimensions == "RE_2", country],
                                            RE == "I am dissatisfied with my relationships" ~ df_utilities_countries[df_utilities_countries$dimensions == "RE_1", country],
                                            RE == "I am very dissatisfied with my relationships" ~ df_utilities_countries[df_utilities_countries$dimensions == "RE_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        DA_u = if("DA" %in% colnames(dimensions)){
                    DA_u = dplyr::case_when(DA == "I am very satisfied with my daily activities" ~ df_utilities_countries[df_utilities_countries$dimensions == "DA_3", country], # DAILY ACTIVITIES
                                            DA == "I am satisfied with my daily activities" ~ df_utilities_countries[df_utilities_countries$dimensions == "DA_2", country],
                                            DA == "I am dissatisfied with my daily activities" ~ df_utilities_countries[df_utilities_countries$dimensions == "DA_1", country],
                                            DA == "I am very dissatisfied with my daily activities" ~ df_utilities_countries[df_utilities_countries$dimensions == "DA_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        PH_u = if("PH" %in% colnames(dimensions)){
                    PH_u = dplyr::case_when(PH == "I have no physical health problems" ~ df_utilities_countries[df_utilities_countries$dimensions == "PH_3", country],  # PHYSICAL HEALTH
                                            PH == "I have some physical health problems" ~ df_utilities_countries[df_utilities_countries$dimensions == "PH_2", country],
                                            PH == "I have many physical health problems" ~ df_utilities_countries[df_utilities_countries$dimensions == "PH_1", country],
                                            PH == "I have a great many physical health problems" ~ df_utilities_countries[df_utilities_countries$dimensions == "PH_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        },
        FU_u = if("FU" %in% colnames(dimensions)){
                    FU_u = dplyr::case_when(FU == "I am very optimistic about my future" ~ df_utilities_countries[df_utilities_countries$dimensions == "FU_3", country], # FUTURE
                                            FU == "I am optimistic about my future" ~ df_utilities_countries[df_utilities_countries$dimensions == "FU_2", country],
                                            FU == "I am gloomy about my future" ~ df_utilities_countries[df_utilities_countries$dimensions == "FU_1", country],
                                            FU == "I am very gloomy about my future" ~ df_utilities_countries[df_utilities_countries$dimensions == "FU_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        }
      )

  }

  if(retain_old_variables == FALSE){
    new_dimensions <- new_dimensions |>
      dplyr::select(dplyr::ends_with("_u"))

    return(new_dimensions)
  }else if(retain_old_variables == TRUE){
    return(new_dimensions)
  }
}

