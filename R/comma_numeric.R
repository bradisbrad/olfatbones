#' Comma Numeric
#'
#' @description Takes a character vector of numbers with commas and returns a numeric vector
#' @param x A character vector
#' @import stringr
#' @return A numeric vector
#' @export comma_numeric
#'
#' @examples
comma_numeric <- function(x){
  x <- str_remove_all(x,",")
  as.numeric(x)
}
