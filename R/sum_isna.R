#' Sum of NA values
#' @param x Integer vector.
#' @param nThread A scalar integer, number of threads.
#' 
#' @export

sum_isna <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("Csum_isna", x, nThread, PACKAGE = packageName())
}

