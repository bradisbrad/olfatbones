#' Insert LHS replacement pipe
#'
#' @return
#' @import magrittr
#' @export
#'
#' @examples
insert_LHSRHS_addin <- function(){
  rstudioapi::insertText(" %<>% ")
}
