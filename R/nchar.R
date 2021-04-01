#' Number of characters
#' @description Various enhancements of \code{nchar} and related operations.
#' 
#' @param x A character vector.
#' 
#' @return
#' \describe{
#' \item{\code{hutilsc_nchar}}{A faster version of base::nchar(x) but only 
#' measures bytes.}
#' \item{\code{range_nchar}}{An length-2 integer vector, equivalent to 
#' \code{range(nchar(x, type = "bytes", keepNA = FALSE))}.}
#' }
#' 
#' @export

hutilsc_nchar <- function(x) {
  stopifnot(is.character(x))
  .Call("Cnchar", x, PACKAGE = packageName())
}


#' @rdname hutilsc_nchar
#' @export
range_nchar <- function(x) {
  stopifnot(is.character(x))
  .Call("Crange_nchar", x, PACKAGE = packageName())
}

