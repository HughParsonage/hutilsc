#' Pad a character vector with leading zeroes
#' @description Useful for ensuring an equilength vector.
#' @param x A character vector.
#' @param w The necessary width; strings in \code{x} where \code{nchar(x) < w}
#' will be padded with the necessary leading zeroes.
#' @export

pad0 <- function(x, w = NULL) {
  stopifnot(is.character(x))
  if (is.null(w)) {
    w <- range_nchar(x)[2]
  }
  w <- ensure_integer(w)
  .Call("Cpad0", x, w, PACKAGE = packageName())
}

