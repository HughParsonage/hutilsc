#' Various sorting functions
#' @name sorted
#' @param x Atomic vector to sort.
#' @param nThread \code{integer(1)} Number of threads to use to verify
#' sortedness.
#' 
#' @export isntSorted

isntSorted <- function(x) {
  if (is.atomic(x)) {
    .Call("Cwhich_isnt_sorted", x, PACKAGE = packageName())
  }
}

#' @rdname sorted
#' @export
is_sorted <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("Cis_sorted", x, nThread, PACKAGE = packageName())
}

unique_sorted <- function(x) {
  .Call("Cunique_sorted", x, PACKAGE = packageName())
}

counting_sort_logi <- function(x) {
  .Call("count_sort_logi", x)
}

