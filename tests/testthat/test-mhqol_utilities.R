###########################################################################
# Tests for mhqol_utility function  ------------------------------
###########################################################################

# Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data
# In the case of states
test_that("Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data (states)", {
  expect_error(mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
                                                                 "I think positively about myself",
                                                                 NA),
                                                          IN = c("I am very satisfied with my level of independence",
                                                                 "I am satisfied with my level of independence",
                                                                 NA),
                                                          MO = c("I do not feel anxious, gloomy, or depressed",
                                                                 "I do not feel anxious, gloomy, or depressed",
                                                                 "I feel anxious, gloomy, or depressed"),
                                                          RE = c("I am very satisfied with my relationships",
                                                                 "I am very satisfied with my relationships",
                                                                 "I am satisfied with my relationships"),
                                                          DA = c("I am very satisfied with my daily activities",
                                                                 "I am very satisfied with my daily activities",
                                                                 "I am satisfied with my daily activities"),
                                                          PH = c("I have no physical health problems",
                                                                 "I have no physical health problems",
                                                                 "I have some physical health problems"),
                                                          FU = c("I am very optimistic about my future",
                                                                 "I am very optimistic about my future",
                                                                 "I am optimistic about my future")), ignore_NA = FALSE))
})


# Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data
# In the case of scores
test_that("Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data (scores)", {
  expect_error(mhqol_utilities(dimensions = data.frame(SI = c(3, 2, NA),
                                                          IN = c(3,1,NA),
                                                          MO = c(3,2,1),
                                                          RE = c(3,2,1),
                                                          DA = c(3,1,2),
                                                          PH = c(1,0,1),
                                                          FU = c(2,1,1)), ignore_NA = FALSE))
})


# Test whether when ignore_na = TRUE, the function does not return an Error but a Warning when there are NA values in the data
#  In the case of states
#  Using the valueset of the Netherlands
test_that("Test whether when ignore_na = TRUE, the function returns no Error but a warning when there are NA values in the data (states)", {
  expect_warning(mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
                                                                   "I think positively about myself",
                                                                   NA),
                                                            IN = c("I am very satisfied with my level of independence",
                                                                   "I am satisfied with my level of independence",
                                                                   NA),
                                                            MO = c("I do not feel anxious, gloomy, or depressed",
                                                                   "I do not feel anxious, gloomy, or depressed",
                                                                   "I feel anxious, gloomy, or depressed"),
                                                            RE = c("I am very satisfied with my relationships",
                                                                   "I am very satisfied with my relationships",
                                                                   "I am satisfied with my relationships"),
                                                            DA = c("I am very satisfied with my daily activities",
                                                                   "I am very satisfied with my daily activities",
                                                                   "I am satisfied with my daily activities"),
                                                            PH = c("I have no physical health problems",
                                                                   "I have no physical health problems",
                                                                   "I have some physical health problems"),
                                                            FU = c("I am very optimistic about my future",
                                                                   "I am very optimistic about my future",
                                                                   "I am optimistic about my future")), ignore_NA = TRUE))
})

# Test whether when ignore_na = TRUE, the function does not return an Error but a Warning when there are NA values in the data
#  In the case of scores
#  Using the valueset of the Netherlands
test_that("Test whether when ignore_na = TRUE, the function returns no Error but a warning when there are NA values in the data (scores)", {
  expect_warning(mhqol_utilities(dimensions = data.frame(SI = c(3, 2, NA),
                                                      IN = c(3,1,NA),
                                                      MO = c(3,2,1),
                                                      RE = c(3,2,1),
                                                      DA = c(3,1,2),
                                                      PH = c(1,0,1),
                                                      FU = c(2,1,1)), ignore_NA = TRUE))
})


# Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data
#  In the case of states
#  Using the valueset of the Netherlands
test_that("Test whether when ignore_invalid = FAlSE, the function returns an Error when there are missing cols in the data (states)", {
  expect_error(mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
                                                                 "I think positively about myself",
                                                                 "I think positively about myself"),
                                                          IN = c("I am very satisfied with my level of independence",
                                                                 "I am satisfied with my level of independence",
                                                                 "I am satisfied with my level of independence"),
                                                          MO = c("I do not feel anxious, gloomy, or depressed",
                                                                 "I do not feel anxious, gloomy, or depressed",
                                                                 "I feel anxious, gloomy, or depressed"),
                                                          RE = c("I am very satisfied with my relationships",
                                                                 "I am very satisfied with my relationships",
                                                                 "I am satisfied with my relationships"),
                                                          DA = c("I am very satisfied with my daily activities",
                                                                 "I am very satisfied with my daily activities",
                                                                 "I am satisfied with my daily activities"),
                                                          FU = c("I am very optimistic about my future",
                                                                 "I am very optimistic about my future",
                                                                 "I am optimistic about my future")), ignore_invalid = FALSE))
})

# Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data
test_that("Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data", {
  expect_error(mhqol_utilities(dimensions = data.frame(SI = c(3,2,1),
                                                          IN = c(3,1,1),
                                                          MO = c(3,2,1),
                                                          RE = c(3,2,1),
                                                          DA = c(3,1,2),
                                                          FU = c(2,1,1)), ignore_invalid = FALSE))
})


# Test whether when ignore_invalid = TRUE, the function returns a warning when there are missing cols in the data
#  In the case of states
#  Using the valueset of the Netherlands
test_that("Test whether when ignore_invalid = TRUE, the function returns an Error when there are missing cols in the data (states)", {
  expect_warning(mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
                                                              "I think positively about myself",
                                                              "I think positively about myself"),
                                                       IN = c("I am very satisfied with my level of independence",
                                                              "I am satisfied with my level of independence",
                                                              "I am satisfied with my level of independence"),
                                                       MO = c("I do not feel anxious, gloomy, or depressed",
                                                              "I do not feel anxious, gloomy, or depressed",
                                                              "I feel anxious, gloomy, or depressed"),
                                                       RE = c("I am very satisfied with my relationships",
                                                              "I am very satisfied with my relationships",
                                                              "I am satisfied with my relationships"),
                                                       DA = c("I am very satisfied with my daily activities",
                                                              "I am very satisfied with my daily activities",
                                                              "I am satisfied with my daily activities"),
                                                       FU = c("I am very optimistic about my future",
                                                              "I am very optimistic about my future",
                                                              "I am optimistic about my future")), ignore_invalid = TRUE))

})

# Test whether when ignore_invalid = TRUE, the function returns a warning when there are missing cols in the data
#  In the case of scores
#  Using the valueset of the Netherlands
test_that("Test whether when ignore_invalid = TRUE, the function returns an Error when there are missing cols in the data (states)", {
  expect_warning(mhqol_utilities(dimensions = data.frame(SI  = c(3,2,1),
                                                                             IN = c(3,1,1),
                                                                             MO = c(3,2,1),
                                                                             RE = c(3,2,1),
                                                                             DA = c(3,1,2),
                                                                             FU = c(2,1,1)), ignore_invalid = TRUE))
})



# Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data
test_that("Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data", {
  df <- mhqol_utilities(dimensions = data.frame(SI = c(3, 2, 1),
                                                   IN = c(3,1,1),
                                                   MO = c(3,2,1),
                                                   RE = c(3,2,1),
                                                   DA = c(3,1,2),
                                                   PH = c(1,0,1),
                                                   FU = c(2,1,1)), retain_old_variables = TRUE)


  expect_true(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))


})


# Test whether when retain_old_variables = FALSE, the function does not return the original variables in the data
test_that("Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data", {
  df <- mhqol_utilities(dimensions = data.frame(SI = c(3, 2, 1),
                                                IN = c(3,1,1),
                                                MO = c(3,2,1),
                                                RE = c(3,2,1),
                                                DA = c(3,1,2),
                                                PH = c(1,0,1),
                                                FU = c(2,1,1)), retain_old_variables = FALSE)


  expect_false(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))


})


# Test whether the function returns the correct values for the dimensions
# In the case of states
# Using the valueset of the Netherlands

test_that("Test whether the function returns the correct states for the scores", {
  df <- mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
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
                                                        "I am very optimistic about my future",
                                                        "I am gloomy about my future",
                                                        "I am very gloomy about my future")))





  expect_equal(df$SI_u, c(0, -0.007, -0.137, -0.211))
  expect_equal(df$IN_u, c(0, -0.018, -0.118, -0.184))
  expect_equal(df$MO_u, c(0, -0.063, -0.179, -0.311))
  expect_equal(df$RE_u, c(0, -0.015, -0.172, -0.269))
  expect_equal(df$DA_u, c(0, -0.021, -0.140, -0.213))
  expect_equal(df$PH_u, c(0, -0.064, -0.243, -0.383))
  expect_equal(df$FU_u, c(0, 0, -0.106, -0.170))

})



# Test whether the function returns the correct values for the dimensions
# In the case of scores
# Using the valueset of the Netherlands

test_that("Test whether the function returns the correct states for the scores", {
  df <- mhqol_utilities(dimensions = data.frame(SI = c("I think very positively about myself",
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
                                                       "I am very optimistic about my future",
                                                       "I am gloomy about my future",
                                                       "I am very gloomy about my future")))





  expect_equal(df$SI_u, c(0, -0.007, -0.137, -0.211))
  expect_equal(df$IN_u, c(0, -0.018, -0.118, -0.184))
  expect_equal(df$MO_u, c(0, -0.063, -0.179, -0.311))
  expect_equal(df$RE_u, c(0, -0.015, -0.172, -0.269))
  expect_equal(df$DA_u, c(0, -0.021, -0.140, -0.213))
  expect_equal(df$PH_u, c(0, -0.064, -0.243, -0.383))
  expect_equal(df$FU_u, c(0, 0, -0.106, -0.170))

})

# Test whether the function returns the correct values for the dimensions
# In the case of scores
# Using the valueset of the Netherlands

test_that("Test whether the function returns the correct states for the scores", {
  df <- mhqol_utilities(dimensions = data.frame(SI = c(3,2,1,0),
                                                IN = c(3,2,1,0),
                                                MO = c(3,2,1,0),
                                                RE = c(3,2,1,0),
                                                DA = c(3,2,1,0),
                                                PH = c(3,2,1,0),
                                                FU = c(3,2,1,0)))





  expect_equal(df$SI_u, c(0, -0.007, -0.137, -0.211))
  expect_equal(df$IN_u, c(0, -0.018, -0.118, -0.184))
  expect_equal(df$MO_u, c(0, -0.063, -0.179, -0.311))
  expect_equal(df$RE_u, c(0, -0.015, -0.172, -0.269))
  expect_equal(df$DA_u, c(0, -0.021, -0.140, -0.213))
  expect_equal(df$PH_u, c(0, -0.064, -0.243, -0.383))
  expect_equal(df$FU_u, c(0, 0, -0.106, -0.170))

})





