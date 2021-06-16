#' PCG Hash
#' @param n Length of the output.
#' @param r Integer vector, typically the random seed. Provide an integer vector
#' as long as \code{.Random.seed} to reproduce results.
#' @param raw_result \code{TRUE | FALSE} Whether or not the output should be
#' type \code{raw}.
#' @param nThread Number of threads to use.
#' 
#' @examples
#' pcg_hash(4)
#' 
#' 
#' @export
pcg_hash <- function(n, r = NULL, raw_result = FALSE, nThread = getOption("hutilsc.nThread", 1L)) {
  if (is.null(r) || !is.integer(r)) {
    r <- .Random.seed
  }
  .Call("Cpcg_hash", as.double(n), r, nThread, raw_result, PACKAGE = packageName()) 
}