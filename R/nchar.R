#' Number of characters
#' @description Various enhancements of \code{nchar} and related operations.
#' 
#' @param x A character vector. Integer vectors are also supported for convenience.
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
  if (is.character(x) || is.integer(x)) {
    .Call("Cnchar", x, PACKAGE = packageName())
  } else {
    nchar(x, type = "bytes")
  }
}


#' @rdname hutilsc_nchar
#' @export
range_nchar <- function(x) {
  if (is.character(x) || is.integer(x)) {
    .Call("Crange_nchar", x, PACKAGE = packageName())
  } else {
    range(nchar(x), na.rm = TRUE)
  }
}

