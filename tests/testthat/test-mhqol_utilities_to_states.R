###########################################################################
# Tests for mhqol_utilities_to_states function  ------------------------------
###########################################################################


# Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data
# In case of the valueset of the Netherlands
test_that("Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data", {
  expect_error(mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, NA),
                                                          IN = c(-0.184, NA),
                                                          MO = c(-0.063, 0),
                                                          RE = c(-0.172, 0),
                                                          DA = c(-0.021, 0),
                                                          PH = c(-0.243, 0),
                                                          FU = c(0, -0.170)),
                                         country = "Netherlands" ,
                                         ignore_NA = FALSE))
})



# Test whether when ignore_na = TRUE, the function does not return an Error but a Warning when there are NA values in the data
# In case of the valueset of the Netherlands
test_that("Test whether when ignore_na = TRUE, the function returns a warning when there are NA values in the data", {
  expect_warning(mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, NA),
                                                                IN = c(-0.184, NA),
                                                                MO = c(-0.063, 0),
                                                                RE = c(-0.172, 0),
                                                                DA = c(-0.021, 0),
                                                                PH = c(-0.243, 0),
                                                                FU = c(0, -0.170)),
                                         country = "Netherlands" ,
                                         ignore_NA = TRUE))
})


# Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data
# In case of the valueset of the Netherlands
test_that("Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data", {
  expect_error(mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, 0),
                                                                IN = c(-0.184, 0),
                                                                MO = c(-0.063, 0),
                                                                DA = c(-0.021, 0),
                                                                PH = c(-0.243, 0),
                                                                FU = c(0, -0.170)),
                                         country = "Netherlands" ,
                                         ignore_invalid = FALSE))
})



# Test whether when ignore_invalid = TRUE, the function does not return an Error but a Warning when there are missing cols in the data
# In case of the valueset of the Netherlands
test_that("Test whether when ignore_invalid = TRUE, he function does not return an Error but a Warning when there are missing cols in the data", {
  expect_warning(mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, 0),
                                                                IN = c(-0.184, 0),
                                                                MO = c(-0.063, 0),
                                                                DA = c(-0.021, 0),
                                                                PH = c(-0.243, 0),
                                                                FU = c(0, -0.170)),
                                         country = "Netherlands" ,
                                         ignore_invalid = TRUE))
})


# Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data
# In case of the valueset of the Netherlands
test_that("Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data", {
  df <- mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, 0),
                                                         IN = c(-0.184, 0),
                                                         MO = c(-0.063, 0),
                                                         RE = c(-0.172, 0),
                                                         DA = c(-0.021, 0),
                                                         PH = c(-0.243, 0),
                                                         FU = c(0, -0.170)),
                                  country = "Netherlands" ,
                                  retain_old_variables = TRUE)


  expect_true(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))


})

# Test whether when retain_old_variables = FALSE, the function does not return the original variables in the data
# In case of the valueset of the Netherlands
test_that("Test whether when retain_old_variables = FALSE, the function does not return the original variables in the data", {
  df <- mhqol_utilities_to_states(utilities = data.frame(SI = c(-0.137, 0),
                                                         IN = c(-0.184, 0),
                                                         MO = c(-0.063, 0),
                                                         RE = c(-0.172, 0),
                                                         DA = c(-0.021, 0),
                                                         PH = c(-0.243, 0),
                                                         FU = c(0, -0.170)),
                                  country = "Netherlands" ,
                                  retain_old_variables = FALSE)


  expect_false(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))
})


# Test whether the function returns the correct scores for the states
test_that("Test whether the function returns the correct states for the scores", {
  df <- mhqol_utilities_to_states(utilities = data.frame(SI = c(0, -0.007, -0.137, -0.211),
                                                   IN = c(0, -0.018, -0.118, -0.184),
                                                   MO = c(0, -0.063, -0.179, -0.311),
                                                   RE = c(0, -0.015, -0.172, -0.269),
                                                   DA = c(0, -0.021, -0.140, -0.213),
                                                   PH = c(0, -0.064, -0.243, -0.383),
                                                   FU = c(0, 0, -0.106, -0.170)),
                                  country = "Netherlands")


  expect_equal(df$SI_s, c("I think very positively about myself",
                          "I think positively about myself",
                          "I think negatively about myself",
                          "I think very negatively about myself"))

  expect_equal(df$IN_s, c("I am very satisfied with my level of independence",
                          "I am satisfied with my level of independence",
                          "I am dissatisfied with my level of independence",
                          "I am very dissatisfied with my level of independence"))

  expect_equal(df$MO_s, c("I do not feel anxious, gloomy, or depressed",
                          "I feel a little anxious, gloomy, or depressed",
                          "I feel anxious, gloomy, or depressed",
                          "I feel very anxious, gloomy, or depressed"))

  expect_equal(df$RE_s, c("I am very satisfied with my relationships",
                          "I am satisfied with my relationships",
                          "I am dissatisfied with my relationships",
                          "I am very dissatisfied with my relationships"))

  expect_equal(df$DA_s, c("I am very satisfied with my daily activities",
                          "I am satisfied with my daily activities",
                          "I am dissatisfied with my daily activities",
                          "I am very dissatisfied with my daily activities"))

  expect_equal(df$PH_s, c("I have no physical health problems",
                          "I have some physical health problems",
                          "I have many physical health problems",
                          "I have a great many physical health problems"))

  expect_equal(df$FU_s, c("I am very optimistic about my future",
                          "I am very optimistic about my future",
                          "I am gloomy about my future",
                          "I am very gloomy about my future"))

})



# Include an warning that in future the utility 0 in the Netherlands can be both I am optimistic about my future


