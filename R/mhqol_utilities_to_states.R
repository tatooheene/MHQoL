#' Provides the states of the MHQoL based on the utilities provided (as described in the valueset)
#'
#' @description
#' This function provides the states of the MHQoL based on the utilities provided (as described in the valueset).
#'
#' @aliases mhqol_utilities_to_states
#'
#' @usage mhqol_utilities_to_states(
#'   utilities,
#'   country = "Netherlands",
#'   ignore_invalid = FALSE,
#'   ignore_NA = FALSE,
#'   retain_old_variables = TRUE)
#'
#' @param utilities A dataframe, numeric vector, or list containing the utilities of the MHQoL.
#'
#' @param country The country for which the utilities should be calculated. For now the only option is "Netherlands".
#'
#' @param ignore_invalid If TRUE, the function will ignore missing utilities and continue processing.
#'
#' @param ignore_NA If TRUE, the function will ignore NA values in the input data.
#'
#' @param retain_old_variables If TRUE, the function will retain the old variables in the output.
#'
#' @return A dataframe containing the states of the MHQoL based on the utilities provided.
#'
#' @keywords MHQoL
#' @keywords States
#' @keywords utilities
#'
#' @examples
#' # Example usage of the mhqol_utilities_to_states function
#' # Get the states based on a numeric vector, not all states are present
#' mhqol_utilities_to_states(utilities = c(IN = -0.018, DA = -0.021,
#' PH = -0.064, FU = -0.106), ignore_invalid = TRUE)
#'
#' # Get the states based on a dataframe
#' mhqol_utilities_to_states(utilities = data.frame(SI = -0.137, IN = -0.184,
#' MO = -0.063, RE = -0.172, DA = -0.021, PH = -0.243, FU = -0.170))

mhqol_utilities_to_states <- function(utilities,
                                      country = "Netherlands",
                                      ignore_invalid = FALSE,
                                      ignore_NA = FALSE,
                                      retain_old_variables = TRUE){



  # Include an warning that in future the utility 0 in the Netherlands can be both I am optimistic about my future
  if(country == "Netherlands"){
    warning("In the Netherlands, the utility 0 in the Future dimension can be both 'I am optimistic about my future' and 'I am very optimistic about my future'")
    utils::flush.console()
  }

  # Convert the different input types into a dataframe
  convert_to_df <- function(utilities){
    # If input is a dataframe
    if(is.data.frame(utilities)){
      return(utilities)
    }

    # If input is a numeric vector
    else if(is.numeric(utilities)){
      if(is.null(names(utilities))){
        stop("Numeric vector must have names for dimension mapping")
      }
      df <- data.frame(matrix(ncol = length(utilities), nrow = 1))
      names(df) <- names(utilities)
      for(dim in names(utilities)){
        df[[dim]] <- utilities[dim]
      }

      return(df)
    }
    # If input is a list
    else if(is.list(utilities)){
      if(is.null(names(utilities))){
        stop("List must have names for dimension mapping")
      }
      return(as.data.frame(utilities, stringsAsFactors = FALSE))
    } else{
      stop("Invalid input type. Please provide a dataframe, numeric vector or list")
    }
  }

  utilities <- convert_to_df(utilities)

  # Required utilities
  required_utilities <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")



  # Check for missing utilities
  missing_utilities <- setdiff(required_utilities, colnames(utilities))



  if(length(missing_utilities) > 0){
    if(ignore_invalid == FALSE){
      stop(paste(
        "The following required utilities are missing:",
        paste(missing_utilities, collapse = ",")
      ))
  } else if(ignore_invalid == TRUE){
    warning(paste(
      "The following required utilities are missing and will be ignored:",
      paste(missing_utilities, collapse = ",")
    ))
  }
}


    # Remove missing utilities from processing
utilities <- utilities[, setdiff(colnames(utilities), missing_utilities), drop = FALSE]



  if(any(is.na(utilities))){
    if(ignore_NA == FALSE){
      stop("The data contains NA values. Please handle NAs or set ignore_NA = TRUE.")
    } else if (ignore_NA == TRUE){
      warning("The data contains NA values. They willl be ignored in processing")
    }
  }


  if(all(sapply(utilities, is.numeric))){
    ## Extract valid values from df_utilities_countries
    valid_utilities <- list(
      SI = c(df_utilities_countries[df_utilities_countries$dimensions == "SI_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "SI_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "SI_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "SI_0", country]),
      IN = c(df_utilities_countries[df_utilities_countries$dimensions == "IN_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "IN_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "IN_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "IN_0", country]),
      MO = c(df_utilities_countries[df_utilities_countries$dimensions == "MO_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "MO_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "MO_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "MO_0", country]),
      RE = c(df_utilities_countries[df_utilities_countries$dimensions == "RE_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "RE_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "RE_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "RE_0", country]),
      DA = c(df_utilities_countries[df_utilities_countries$dimensions == "DA_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "DA_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "DA_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "DA_0", country]),
      PH = c(df_utilities_countries[df_utilities_countries$dimensions == "PH_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "PH_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "PH_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "PH_0", country]),
      FU = c(df_utilities_countries[df_utilities_countries$dimensions == "FU_3", country],
             df_utilities_countries[df_utilities_countries$dimensions == "FU_2", country],
             df_utilities_countries[df_utilities_countries$dimensions == "FU_1", country],
             df_utilities_countries[df_utilities_countries$dimensions == "FU_0", country])

    )


    # Function to validate utilities before transformation
    validate_utilities <- function(df, valid_utilities) {
      for (col in names(valid_utilities)) {  # Loop only over expected columns
        if (col %in% colnames(df)) {
          # Check if values are within valid ranges (using near() for numerical stability)
          invalid_values <- df[[col]][
            sapply(df[[col]], function(x) !any(dplyr::near(x, valid_utilities[[col]])) & !is.na(x))
          ]

          # Stop if invalid values are found
          if (length(invalid_values) > 0) {
            stop(paste("Error: Column", col, "contains unexpected values:", paste(invalid_values, collapse = ", ")))
          }
        }
      }
    }


    validate_utilities(utilities, valid_utilities)



    new_utilities <- utilities |>
      dplyr::mutate(
        SI_s = if("SI" %in% colnames(utilities)){
          dplyr::case_when(SI == df_utilities_countries[df_utilities_countries$dimensions =="SI_3", country] ~ "I think very positively about myself", # SELF-IMAGE
                           SI == df_utilities_countries[df_utilities_countries$dimensions =="SI_2", country] ~ "I think positively about myself",
                           SI == df_utilities_countries[df_utilities_countries$dimensions =="SI_1", country] ~ "I think negatively about myself",
                           SI == df_utilities_countries[df_utilities_countries$dimensions =="SI_0", country] ~ "I think very negatively about myself",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        IN_s = if("IN" %in% colnames(utilities)){
          dplyr::case_when(IN == df_utilities_countries[df_utilities_countries$dimensions =="IN_3", country] ~ "I am very satisfied with my level of independence", # INDEPENDENCE
                           IN == df_utilities_countries[df_utilities_countries$dimensions =="IN_2", country] ~ "I am satisfied with my level of independence",
                           IN == df_utilities_countries[df_utilities_countries$dimensions =="IN_1", country] ~ "I am dissatisfied with my level of independence",
                           IN == df_utilities_countries[df_utilities_countries$dimensions =="IN_0", country] ~ "I am very dissatisfied with my level of independence",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        MO_s = if("MO" %in% colnames(utilities)){

          dplyr::case_when(MO == df_utilities_countries[df_utilities_countries$dimensions =="MO_3", country] ~ "I do not feel anxious, gloomy, or depressed",    # MOOD
                           MO == df_utilities_countries[df_utilities_countries$dimensions =="MO_2", country] ~ "I feel a little anxious, gloomy, or depressed",
                           MO == df_utilities_countries[df_utilities_countries$dimensions =="MO_1", country] ~ "I feel anxious, gloomy, or depressed",
                           MO == df_utilities_countries[df_utilities_countries$dimensions =="MO_0", country] ~ "I feel very anxious, gloomy, or depressed",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        RE_s = if("RE" %in% colnames(utilities)){
          dplyr::case_when(RE == df_utilities_countries[df_utilities_countries$dimensions =="RE_3", country]~ "I am very satisfied with my relationships",     # RELATIONSHIPS
                           RE == df_utilities_countries[df_utilities_countries$dimensions =="RE_2", country] ~ "I am satisfied with my relationships",
                           RE == df_utilities_countries[df_utilities_countries$dimensions =="RE_1", country] ~ "I am dissatisfied with my relationships",
                           RE == df_utilities_countries[df_utilities_countries$dimensions =="RE_0", country] ~  "I am very dissatisfied with my relationships",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        DA_s = if("DA" %in% colnames(utilities)){
          dplyr::case_when(DA == df_utilities_countries[df_utilities_countries$dimensions =="DA_3", country] ~ "I am very satisfied with my daily activities",  # DAILY ACTIVITIES
                           DA == df_utilities_countries[df_utilities_countries$dimensions =="DA_2", country] ~ "I am satisfied with my daily activities",
                           DA == df_utilities_countries[df_utilities_countries$dimensions =="DA_1", country] ~ "I am dissatisfied with my daily activities",
                           DA == df_utilities_countries[df_utilities_countries$dimensions =="DA_0", country] ~ "I am very dissatisfied with my daily activities",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        PH_s = if("PH" %in% colnames(utilities)){
          dplyr::case_when(PH == df_utilities_countries[df_utilities_countries$dimensions =="PH_3", country] ~ "I have no physical health problems",  # PHYSICAL HEALTH
                           PH == df_utilities_countries[df_utilities_countries$dimensions =="PH_2", country] ~ "I have some physical health problems",
                           PH == df_utilities_countries[df_utilities_countries$dimensions =="PH_1", country] ~ "I have many physical health problems" ,
                           PH == df_utilities_countries[df_utilities_countries$dimensions =="PH_0", country] ~  "I have a great many physical health problems",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        },
        FU_s = if("FU" %in% colnames(utilities)){
          dplyr::case_when(FU == df_utilities_countries[df_utilities_countries$dimensions =="FU_3", country] ~ "I am very optimistic about my future", # FUTURE
                           FU == df_utilities_countries[df_utilities_countries$dimensions =="FU_2", country] ~ "I am optimistic about my future",
                           FU == df_utilities_countries[df_utilities_countries$dimensions =="FU_1", country] ~ "I am gloomy about my future",
                           FU == df_utilities_countries[df_utilities_countries$dimensions =="FU_0", country] ~ "I am very gloomy about my future",
                           TRUE ~ NA_character_)
        }else{
          NA_character_
        }
      )


  } else{
    stop("All utilities must be numeric")
  }

if(retain_old_variables == FALSE){
  new_utilities <- new_utilities |>
    dplyr::select(dplyr::ends_with("_s"))

  return(new_utilities)
}else if(retain_old_variables == TRUE){
  return(new_utilities)
}




}
