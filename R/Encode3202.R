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
  .Call("CEncode3202", x, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Decode3202 <- function(enc) {
  stopifnot(is.integer(enc))
  .Call("CDecode3202", enc, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Validate3202 <- function(x) {
  stopifnot(is.character(x))
  .Call("CValidate3202", x, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
Lookup4 <- function(enc) {
  .Call("lookup4_char", enc, PACKAGE = packageName())
}

#' @rdname Encode3202
#' @export
names2int <- function(x, y) {
  .Call("Cnames2int", x, y, PACKAGE = packageName())
}

CountRecordID <- function(x) {
  .Call("CCountRecordID", x, PACKAGE = packageName())
}

classify_chars <- function(x, n) {
  .Call("Cclassify_chars", x, n, PACKAGE = packageName())
}

EncodeRecordID <- function(x) {
  .Call("CencodeRecordID", x, PACKAGE = packageName())
}

DecodeRecordID <- function(x) {
  .Call("CdecodeRecordID", x, PACKAGE = packageName())
}

pad0 <- function(x, w) {
  stopifnot(is.character(x), is.integer(w))
  .Call("Cpad0", x, w, PACKAGE = packageName())
}

tabula_RecordID <- function(x) {
  o <- .Call("Ctabula_RecordID", x, PACKAGE = packageName())
  as.data.table(matrix(o, ncol = 19))[, c := c(0:9, LETTERS, letters)]
}

