#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' @import english
#'
#' @export

make_phrase <- function(num, num_word, item, verb, adjective, location){

  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")

  num_word <- english::english(num)

  if (num == 1) {
    num_word <- "a"
  } else {
    item <- pluralize_gift(item)
  }

  sentence <- paste(num_word, adjective, item, verb, location)

  sentence <- sentence %>%
    str_replace_all("\\s+", " ") %>%
    str_trim("both")

  return(sentence)

}

# Unit Test
test_that("the num arguement in the make_phrase function should be numeric", {
  expect_error(make_phrase(num = "hello", 
                           num_word = "six", 
                           item = "ballon", 
                           verb = "flying", 
                           adjective = "green", 
                           location = "up in the sky"))
})
