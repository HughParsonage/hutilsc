#' Is a vector constant 
#' 
#' @param x An atomic vector.
#' @param nThread Number of threads to use.
#' @export

is_constant <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_is_constant", x, nThread, PACKAGE = packageName())
}
