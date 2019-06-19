#' Reorder Correlation Matrix by correlation
#'
#' @param cormat
#'
#' @return matrix
#' @export reorder_cormat
#'
#' @examples
#' cormat <- round(cor(mtcars), 2)
#' reorder_cormat(cormat)
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
