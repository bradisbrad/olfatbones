#' Split Numeric
#'
#' @description Takes a character vector of numbers with commas and returns a numeric vector
#' @param x A character vector
#' @param split_char The character to split by, as some regions use periods instead of commas (defaults to ",")
#' @import stringr
#' @return A numeric vector
#' @export split_numeric
#'
#' @examples
#' comma_vec <- c("100,000", "250,000", "1,250", "4,242.42")
#' split_numeric(comma_vec)
#'
#' period_vec <- c("100.000", "250.000", "1.250", "4.242,42")
#' split_numeric(period_vec, split_char = ".")
#'
split_numeric <- function(x, split_char = ","){
  if(split_char == "."){
    split_char <- "\\."
  }
  x <- str_remove_all(x,split_char) %>%
    str_replace(",", ".")
  as.numeric(x)
}
