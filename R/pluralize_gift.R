#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import english
#'
#' @export
pluralize_gift <- function(gift){

    if (str_detect(gift, "oo") == TRUE) {
      return(str_replace(gift, "oo", "ee"))
  } else if (str_detect(gift, "$y") == TRUE) {
      return(str_replace("$y", "ies"))
  } else {
      return(paste0(gift, "s"))
  }

}
