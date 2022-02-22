library(testthat)
library(twelvedays)

test_check("twelvedays")

test_that("the pluralize_gift function should only have one argument", {
  expect_error(pluralize_gift("bunny", "carrot"))
})

test_that("the num arguement in the make_phrase function should be numeric", {
  expect_error(make_phrase(num = "hello", 
                           num_word = "six", 
                           item = "ballon", 
                           verb = "flying", 
                           adjective = "green", 
                           location = "up in the sky"))
})

test_that("all of the arguments in the sing_line function are required, not optional", {
  expect_error(sing_day(line = 5, phrase_col = Full.Phrase))
})
