#' Sum a raw vector
#' @description In particular when using the \code{unsigned char}
#' to represent a bool.
#' 
#' @param x A raw atomic vector.
#' @param nThread Number of threads to use.
#' 
#' See also \code{bit::bit}.
#' 
#' @return Summarizes a raw vector:
#' \describe{
#' \item{\code{sum_raw}}{The sum of \code{x} as if the elements were unsigned integers. 
#' Returns \code{NULL} if \code{x} is not raw.}
#' \item{\code{tabulate_raw}}{A length-256 integer vector tabulating the number of 
#' raw elements. }
#' }
#' 
#' @examples
#' x <- c(as.raw(0), as.raw(1))
#' sum_raw(x)
#' tabulate_raw(x)
#' 
#' x <- c(as.raw(0), as.raw(1))
#' sum_raw(x)
#' tabulate_raw(x)
#' 
#' @export
sum_raw <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("CSumRaw", x, nThread, PACKAGE = packageName())
}

#' @rdname sum_raw
#' @export
tabulate_raw <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("CCountRaws", x, nThread, PACKAGE = packageName())
}


