#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import english
#'
#' @export
sing_day <- function(dataset, line, phrase_col){

  word_number <- english::ordinal(line)

  day_line <- paste("On the", word_number, "day of Christmas, my true love sent to me:")

  phrases <- dataset %>% pull({{phrase_col}})

  if (line > 1) {
    phrases[1] <- paste0("and ", phrases[1], ".")
  } else {
    phrases[1] <- paste0(phrases[1], ".")
  }

  gift_line <- map_chr(line:1, ~paste0(phrases[.x], sep = ","))

  combined <- c(day_line, gift_line)

  combined <- paste0(combined, sep = "\n", collapse = " ")

  combined <- combined %>%
    str_remove(",$")

  cat(combined)

}
