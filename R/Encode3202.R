#' Encode 
#' @param x,y \code{character(n)} Vectors to encode.
#' @param enc \code{integer(n)} Encoded vectors to decode.
#' 
#' @return \code{Encode3202} returns an integer based on the digits following
#' the prefix \code{"3202"}. \code{Decode3202} is the reverse process.
#' 
#' \code{names2int} takes two character vectors of equal length and returns
#' an integer based on the first two digits. \code{Lookup4} is the inverse
#' process, returning the first two characters of each original vector
#' in \code{AaAa} case.
#' 
#' 
#' @export

Encode3202 <- function(x) {
  .Call("Encode3202", x, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Decode3202 <- function(enc) {
  stopifnot(is.integer(enc))
  .Call("Decode3202", enc, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Lookup2 <- function(x) {
  .Call("lookup2_char", x, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Lookup4 <- function(enc) {
  .Call("lookup4_char", enc, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
names2int <- function(x, y) {
  .Call("names2int", x, y, PACKAGE = packageName())
}
