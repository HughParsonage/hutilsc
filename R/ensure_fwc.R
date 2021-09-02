#' Ensure character vector is fixed width
#' @param x A character vector.
#' @return
#' If each element of \code{x} has the same number of characters,
#' \code{x} is returned.
#' 
#' Otherwise, each element narrower of the maximal width is padded with 
#' leading zeroes.
#' 
#' @export

ensure_fwc <- function(x) {
  .Call("CEnsure_fwc", x, PACKAGE = packageName())
}
