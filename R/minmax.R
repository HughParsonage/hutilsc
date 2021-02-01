#' Minimum and maximum 
#' 
#' @param x A vector for which the minimum and maximum are desired.
#' @param nThread An integer, the number of threads to use.
#' 
#' 
#' @export

minmax <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_minmax", x, x[0], nThread, PACKAGE = packageName())
}
