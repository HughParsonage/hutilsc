#' Various sorting functions
#' @name sorted
#' @param x Atomic vector to sort.
#' @param nThread \code{integer(1)} Number of threads to use to verify
#' sortedness.
#' 
#' @export isntSorted

isntSorted <- function(x) {
  if (is.atomic(x)) {
    .Call("do_which_isnt_sorted", x, PACKAGE = packageName())
  }
}

#' @rdname sorted
#' @export
is_sorted <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_is_sorted", x, nThread, PACKAGE = packageName())
}



