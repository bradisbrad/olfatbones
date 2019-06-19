#' Triangularize a Correlation Matrix
#' @description Takes a correlation matrix and removes the redundant information
#' @param cormat Correlation Matrix
#' @param lower Lower or upper half (default = T) select F for upper half
#'
#' @return Matrix
#' @export get_tri
#'
#' @examples
#' cormat <- round(cor(mtcars), 2)
#' get_tri(cormat)
#'
get_tri <- function(cormat, lower = T){
  if(lower){
    cormat[upper.tri(cormat)] <- NA
  } else {
    cormat[lower.tri(cormat)] <- NA
  }
  cormat
}
