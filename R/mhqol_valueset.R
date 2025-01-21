#' Provides the valueset of the MHQoL based on the country specified
#'
#' @description
#' This function provides the valueset of the MHQoL based on the country specified.
#'
#' @param country A character value indicating the country for which the valueset should be provided.
#' For now only the Netherlands is available.
#'
#' @return A dataframe containing the valueset of the MHQoL based on the country specified.
#'
#' @keywords MHQoL, Valueset, Country
#'
#' @examples
#' # Example usage of the mhqol_valueset function
#' # Get the valueset for the Netherlands
#' mhqol_valueset(country = "Netherlands")

mhqol_valueset <- function(country = "Netherlands"){

# Read in the data frame  # DIT AANPASSEN
df_utilities_countries <- readRDS("data/df_utilities_countries.RDS")

# Check if the country is a single value
if (length(country) != 1) {
  stop("The 'country' argument must be a single value. Please choose a country from the list of countries.")
}

# Check if the country is a column in the data frame
if (!country %in% colnames(df_utilities_countries)) {
  stop("Invalid country chosen. Please choose a country from the list of countries.")
}

# Filter the data frame based on the country
df_utilities_country <- df_utilities_countries |>
  dplyr::select(dimensions, dplyr::all_of(country))

# Return the data frame
return(df_utilities_country)
}

