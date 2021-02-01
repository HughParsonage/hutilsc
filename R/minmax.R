#' Minimum and maximum 
#' 
#' @param x A vector for which the minimum and maximum are desired.
#' @param empty_result The return value if \code{length(x) == 0}.
#' @param nThread An integer, the number of threads to use.
#' 
#' @return A length-two vector, the minimum and maximum of \code{x}. If 
#' \code{typeof(x)} is not supported, \code{NULL}.
#' 
#' @export

minmax <- function(x, empty_result = NULL, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_minmax", x, empty_result, nThread, PACKAGE = packageName())
}
