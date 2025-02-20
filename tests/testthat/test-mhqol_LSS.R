###########################################################################
# Tests for mhqol_LSS function  ------------------------------
###########################################################################

# Test whether the function returns an error when there is a score below zero in the data
test_that("Test whether the function returns an error when there is a score below zero in the data", {
  expect_error(mhqol_LSS(scores = data.frame(SI = c(3,2,1,0),
                                                          IN = c(3,2,1,0),
                                                          MO = c(3,2,1,0),
                                                          RE = c(3,2,1,0),
                                                          DA = c(3,2,1,0),
                                                          PH = c(3,2,1,-1),
                                                          FU = c(3,2,1,0))))

})

# Test whether the function returns an error when there is a score above 3 in the data
test_that("Test whether the function returns an error when there is a score above 3 in the data", {
  expect_error(mhqol_scores_to_states(scores = data.frame(SI = c(3,2,1,0),
                                                          IN = c(3,2,1,0),
                                                          MO = c(3,2,1,0),
                                                          RE = c(3,2,1,0),
                                                          DA = c(3,2,1,0),
                                                          PH = c(3,2,1,4),
                                                          FU = c(3,2,1,0))))

})


# Test whether the function returns no error when the scores are between 0 - 3
test_that("Test whether the function returns no  error when the scores are between 0 and 3 in the data", {
  expect_no_error(mhqol_scores_to_states(scores = data.frame(SI = c(3,2,1,0),
                                                          IN = c(3,2,1,0),
                                                          MO = c(3,2,1,0),
                                                          RE = c(3,2,1,0),
                                                          DA = c(3,2,1,0),
                                                          PH = c(3,2,1,0),
                                                          FU = c(3,2,1,0))))

})

# Test whether the outcome of the total LSS is correct in the case of a provided dataset with scores
test_that("Test whether the outcome of the total LSS is correct in the case of a provided dataset with scores", {
  df <- mhqol_LSS(dimensions = data.frame(SI = c(3,2,1,0),
                                      IN = c(1,3,1,0),
                                      MO = c(0,0,0,0),
                                      RE = c(3,2,1,0),
                                      DA = c(1,2,1,0),
                                      PH = c(0,2,1,0),
                                      FU = c(3,1,1,0)), metric = "total")

  expect_equal(df$LSS, c(11, 12, 6, 0))



})

# Test whether the outcome of the average LSS is correct in the case of a provided dataset with scores
test_that("Test whether the outcome of the average LSS is correct in the case of a provided dataset with scores", {
  df <- mhqol_LSS(dimensions = data.frame(SI = c(3,2,1,0),
                                          IN = c(1,3,1,0),
                                          MO = c(0,0,0,0),
                                          RE = c(3,2,1,0),
                                          DA = c(1,2,1,0),
                                          PH = c(0,2,1,0),
                                          FU = c(3,1,1,0)), metric = "average")

  expect_equal(df$LSS, 7.25)



})


# Test whether the outcome of the total LSS is correct in the case of a provided dataset with states
test_that("Test whether the outcome of the total LSS is correct in the case of a provided dataset with states", {
  df <- mhqol_LSS(dimensions = data.frame(SI = c("I think very positively about myself",
                                                 "I think positively about myself",
                                                 "I think negatively about myself",
                                                 "I think very negatively about myself"),
                                          IN = c("I am dissatisfied with my level of independence",
                                                 "I am very satisfied with my level of independence",
                                                 "I am dissatisfied with my level of independence",
                                                 "I am very dissatisfied with my level of independence"),
                                          MO = c("I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed"),
                                          RE = c("I am very satisfied with my relationships",
                                                 "I am satisfied with my relationships",
                                                 "I am dissatisfied with my relationships",
                                                 "I am very dissatisfied with my relationships"),
                                          DA = c("I am dissatisfied with my daily activities",
                                                 "I am satisfied with my daily activities",
                                                 "I am dissatisfied with my daily activities",
                                                 "I am very dissatisfied with my daily activities"),
                                          PH = c("I have a great many physical health problems",
                                                 "I have some physical health problems",
                                                 "I have many physical health problems",
                                                 "I have a great many physical health problems"),
                                          FU = c("I am very optimistic about my future",
                                                 "I am gloomy about my future",
                                                 "I am gloomy about my future",
                                                 "I am very gloomy about my future")), metric = "total")




  expect_equal(df$LSS, c(11, 12, 6, 0))



})

# Test whether the outcome of the average LSS is correct in the case of a provided dataset with states
test_that("Test whether the outcome of the average LSS is correct in the case of a provided dataset with states", {
  df <- mhqol_LSS(dimensions = data.frame(SI = c("I think very positively about myself",
                                                 "I think positively about myself",
                                                 "I think negatively about myself",
                                                 "I think very negatively about myself"),
                                          IN = c("I am dissatisfied with my level of independence",
                                                 "I am very satisfied with my level of independence",
                                                 "I am dissatisfied with my level of independence",
                                                 "I am very dissatisfied with my level of independence"),
                                          MO = c("I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed",
                                                 "I feel very anxious, gloomy, or depressed"),
                                          RE = c("I am very satisfied with my relationships",
                                                 "I am satisfied with my relationships",
                                                 "I am dissatisfied with my relationships",
                                                 "I am very dissatisfied with my relationships"),
                                          DA = c("I am dissatisfied with my daily activities",
                                                 "I am satisfied with my daily activities",
                                                 "I am dissatisfied with my daily activities",
                                                 "I am very dissatisfied with my daily activities"),
                                          PH = c("I have a great many physical health problems",
                                                 "I have some physical health problems",
                                                 "I have many physical health problems",
                                                 "I have a great many physical health problems"),
                                          FU = c("I am very optimistic about my future",
                                                 "I am gloomy about my future",
                                                 "I am gloomy about my future",
                                                 "I am very gloomy about my future")), metric = "average")




  expect_equal(df$LSS, 7.25)



})



