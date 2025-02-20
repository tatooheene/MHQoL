###########################################################################
# Tests for mhqol function  ------------------------------
###########################################################################

# Test whether the function returns the correct total values for the dimensions per participant
# In the case of states
# Using the valueset of the Netherlands

test_that("Test whether the function returns the correct total utility for the states", {
  df <- mhqol(dimensions = data.frame(SI = c("I think very positively about myself",
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
                                                       "I am very gloomy about my future")),
              metric = "total")





  expect_equal(df$utility, c(1, 0.812, -0.095, -0.741))

})

# Test whether the function returns the correct total values for the dimensions per participant
# In the case of scores
# Using the valueset of the Netherlands
test_that("Test whether the function returns the correct total utility for the scores", {
  df <- mhqol(dimensions = data.frame(SI = c(3,2,1,0),
                                      IN = c(3,2,1,0),
                                      MO = c(3,2,1,0),
                                      RE = c(3,2,1,0),
                                      DA = c(3,2,1,0),
                                      PH = c(3,2,1,0),
                                      FU = c(3,2,1,0)),
              metric = "total")





  expect_equal(df$utility, c(1, 0.812, -0.095, -0.741))

})

# Test whether the function returns the correct average values for the dimensions per participant
# In the case of states
# Using the valueset of the Netherlands
test_that("Test whether the function returns the correct average utility for the states", {
  df <- mhqol(dimensions = data.frame(SI = c("I think very positively about myself",
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
                                             "I am very gloomy about my future")),
              metric = "average")





  expect_equal(df$utility, 0.244)

})


# Test whether the function returns the correct average values for the dimensions per participant
# In the case of scores
# Using the valueset of the Netherlands
test_that("Test whether the function returns the correct average utility for the scores", {
  df <- mhqol(dimensions = data.frame(SI = c(3,2,1,0),
                                      IN = c(3,2,1,0),
                                      MO = c(3,2,1,0),
                                      RE = c(3,2,1,0),
                                      DA = c(3,2,1,0),
                                      PH = c(3,2,1,0),
                                      FU = c(3,2,1,0)),
              metric = "average")

  expect_equal(df$utility, 0.244)
})



