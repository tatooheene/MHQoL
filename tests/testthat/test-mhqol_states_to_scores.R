###########################################################################
# Tests for mhqol_states_to_scores function  ------------------------------
###########################################################################


# Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data
test_that("Test whether when ignore_na = FAlSE, the function returns an Error when there are NA values in the data", {
  expect_error(mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                                 "I have physical health problems"),
                                                          FU = c("I am very optimistic about my future",
                                                                 "I am very optimistic about my future",
                                                                 "I am optimistic about my future")), ignore_NA = FALSE))
})



# Test whether when ignore_na = TRUE, the function does not return an Error but a Warning when there are NA values in the data
test_that("Test whether when ignore_na = TRUE, the function returns no Error but a warning when there are NA values in the data", {
  expect_warning(mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                                 "I have many physical health problems"),
                                                          FU = c("I am very optimistic about my future",
                                                                 "I am very optimistic about my future",
                                                                 "I am optimistic about my future")), ignore_NA = TRUE))
})


# Test whether when ignore_invalid = FALSE, the function returns an Error when there are missing cols in the data

test_that("Test whether when ignore_invalid = FAlSE, the function returns an Error when there are missing cols in the data", {
  expect_error(mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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


# Test whether when ignore_invalid = TRUE, the function does not return an Error but a Warning when there are missing cols in the data
test_that("Test whether when ignore_invalid = TRUE, the function does not return an Error but a Warning when there are missing cols in the data", {
  expect_warning(mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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


# Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data
test_that("Test whether when retain_old_variables = TRUE, the function also returns the original variables in the data", {
  df <- mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                   PH = c("I have no physical health problems",
                                                          "I have no physical health problems",
                                                          "I have some physical health problems"),
                                                   FU = c("I am very optimistic about my future",
                                                          "I am very optimistic about my future",
                                                          "I am optimistic about my future")), retain_old_variables = TRUE)


  expect_true(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))


})

# Test whether when retain_old_variables = FALSE, the function does not return the original variables in the data
test_that("Test whether when retain_old_variables = FALSE, the function does not return the original variables in the data", {
  df <- mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                   PH = c("I have no physical health problems",
                                                          "I have no physical health problems",
                                                          "I have some physical health problems"),
                                                   FU = c("I am very optimistic about my future",
                                                          "I am very optimistic about my future",
                                                          "I am optimistic about my future")), retain_old_variables = FALSE)


  expect_false(all(c("SI", "IN", "MO", "RE", "DA", "PH", "FU") %in% colnames(df)))
})


# Test whether the function returns the correct scores for the states
test_that("Test whether the function returns the correct scores for the states", {
  df <- mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                          "I am very gloomy about my future")))

  expect_equal(df$SI_s, c(3, 2, 1, 0))
  expect_equal(df$IN_s, c(3, 2, 1, 0))
  expect_equal(df$MO_s, c(3, 2, 1, 0))
  expect_equal(df$RE_s, c(3, 2, 1, 0))
  expect_equal(df$DA_s, c(3, 2, 1, 0))
  expect_equal(df$PH_s, c(3, 2, 1, 0))
  expect_equal(df$FU_s, c(3, 2, 1, 0))
})

# Test whether the function returns an error when there is a non answer in the data
test_that("Test whether the function returns an error when there is a non answer in the data", {
  expect_error(mhqol_states_to_scores(states = data.frame(SI = c("I think very positively about myself",
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
                                                                 "Test"))))
})
