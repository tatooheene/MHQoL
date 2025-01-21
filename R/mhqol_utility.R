#' Provides the scores of the MHQoL based on the textual input (as described in the manual)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function provides the utilities of the MHQoL based on
#' textual, as described in the manual, or numeric input of the MHQoL.
#'
#' @param dimensions A dataframe, character vector, numeric vector, or list containing
#' the character or numeric dimensions of the MHQoL.
#' Must contain the following dimensions: SI (Self-Image), IN (INdependence),
#' MO (MOod), RE (RElationships), DA (Daily Activities), PH (Physical Health), FU (FUture).
#'
#' @param country The country for which the utilities should be calculated. For now the only option is "Netherlands".
#'
#' @param ignore.invalid If TRUE, the function will ignore missing dimensions and continue processing.
#' If FALSE, the function will stop and throw an error.
#'
#' @param ignore.NA If TRUE, the function will ignore NA values in the input data.
#' If FALSE, the function will stop and throw an error.
#'
#' @param retain_old_variables If TRUE, the function will return the original dimensions along with the new utilities.
#' If FALSE, the function will only return the new utilities.
#'
#' @return A dataframe containing the new utilities based on the MHQoL manual.
#'
#' @keywords MHQoL, Utilities, Dimensions
#'
#' @examples
#' # Example usage of the mhqol_utilities function
#' # Get the MHQoL utilities based on a character vector and do not retain old values
#' mhqol_utilities(dimensions = c(SI = "I think very positively about myself",
#' IN = "I am very satisfied with my level of independence",
#' MO = "I do not feel anxious, gloomy, or depressed",
#' RE = "I am very satisfied with my relationships",
#' DA = "I am very satisfied with my daily activities",
#' PH = "I have no physical health problems",
#' FU = "I am very optimistic about my future"), retain_old_variables = FALSE))
#'
#' # Get the MHQoL utilities based on a DataFrame and retain old values
#' mhqol_utilities(dimensions = df)
#'
#' # Get the MHQoL utilities based on a DataFrame and ignore missing dimensions
#' mhqol_utilities(dimensions = df, ignore.invalid = TRUE)
#'
#' Get the MHQoL utilities based on a numeric vector and ignore missing dimensions
#' mhqol_utilities(dimensions = c(IN = 2, MO = 1, RE = 0, DA = 3, PH = 2, FU = 1), ignore.invalid = TRUE)



mhqol_utilities <- function(dimensions,
                            country = "Netherlands",
                            ignore.invalid = FALSE,
                            ignore.NA = TRUE,
                            retain_old_variables = TRUE) {

  # Read in utility dataset
  df_utilities_countries <- readRDS("data/df_utilities_countries.RDS") ##AANPASSEN

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
    if(ignore.invalid == FALSE)
      stop(paste(
        "The following required dimensions are missing:",
        paste(missing_dimensions, collapse = ",")
      ))
  } else if(ignore.invalid == TRUE){
    warning(paste(
      "The following required dimensions are missing and will be ignored:",
      paste(missing_dimensions, collapse = ",")
    ))

    # Remove missing dimensions from processing
    dimensions <-  setdiff(required_dimensions, missing_dimensions)
  }



  if(any(is.na(dimensions))){
    if(ignore.NA == FALSE){
      stop("The data contains NA values. Please handle NAs or set ignore.NA = TRUE.")
    } else if (ignore.NA == TRUE){
      warning("The data contains NA values. They willl be ignored in processing")
    }
  }

  if(all(sapply(dimensions, is.numeric))){
    new_dimensions <- dimensions |>
      dplyr::mutate(
        SI_u = if("SI" %in% colnames(dimensions)){
        dplyr::case_when(SI == 3 ~  df_utilities_countries[df_utilities_countries$dimensions =="SI_3", country], # SELF-IMAGE
                                            SI == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_2", country],
                                            SI == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_1", country],
                                            SI == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="SI_0", country],
                         TRUE ~ NA_real_)
        }else{
          NA_real_
        },
        IN_u = if("IN" %in% colnames(dimensions)){
                    dplyr::case_when(IN == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_3", country], # INDEPENDENCE
                                            IN == 2  ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_2", country],
                                            IN == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_1", country],
                                            IN == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="IN_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        MO_u = if("MO" %in% colnames(dimensions)){
                    dplyr::case_when(MO == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_3", country],    # MOOD
                                            MO == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_2", country],
                                            MO == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_1", country],
                                            MO == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="MO_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        RE_u = if("RE" %in% colnames(dimensions)){
                    RE_u = dplyr::case_when(RE == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_3", country],     # RELATIONSHIPS
                                            RE == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_2", country],
                                            RE == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_1", country],
                                            RE == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="RE_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        DA_u = if("DA" %in% colnames(dimensions)){
                    DA_u = dplyr::case_when(DA == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_3", country],  # DAILY ACTIVITIES
                                            DA == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_2", country],
                                            DA == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_1", country],
                                            DA == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="DA_0", country],
                                            TRUE~NA_real_)
        }else{
          NA_real_
        },
        PH_u = if("PH" %in% colnames(dimensions)){
                    PH_u = dplyr::case_when(PH == 3 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_3", country],  # PHYSICAL HEALTH
                                            PH == 2 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_2", country],
                                            PH == 1 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_1", country],
                                            PH == 0 ~ df_utilities_countries[df_utilities_countries$dimensions =="PH_0", country],
                                     TRUE~NA_real_)
        }else{
          NA_real_
        },
        FU_u = if("FU" %in% colnames(dimensions)){
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
                                            MO == "I feel very anxious, gloomy, or dperessed" ~ df_utilities_countries[df_utilities_countries$dimensions == "MO_0", country],
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
