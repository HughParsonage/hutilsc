#' Find singleton missing value
#' @description Demo of \code{xor} detection of absent value.
#' @param x An integer vector, a proper subset of \code{1:N} with 
#' \code{N == length(x) + 1}. 
#' 
#' @examples
#' A <- sample.int(1003, size = 1002)
#' setdiff(1:1003, A)
#' findAbsent(A)
#' 
#' @export
findAbsent <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  # return the singleton setdiff(seq_len(length(x) + 1), x)
  .Call("CfindAbsent", x, nThread, PACKAGE = packageName())
}
