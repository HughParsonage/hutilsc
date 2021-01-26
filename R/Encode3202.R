#' Encode 
#' @param x Character vector
#' 
#' @export

Encode3202 <- function(x) {
  .Call("Encode3202", x, PACKAGE = packageName())
}

#' @export
Decode3202 <- function(x) {
  stopifnot(is.integer(x))
  .Call("Decode3202", x, PACKAGE = packageName())
}

#' @export
Lookup2 <- function(x) {
  .Call("lookup2_char", x, PACKAGE = packageName())
}
#' @export
Lookup4 <- function(x) {
  .Call("lookup4_char", x, PACKAGE = packageName())
}

#' @export
names2int <- function(x, y) {
  .Call("names2int", x, y, PACKAGE = packageName())
}
