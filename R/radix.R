

test_radix_find <- function(x, tbl, x0 = 0L) {
  stopifnot(is.integer(x), length(x) == 1, !is.unsorted(tbl))
  .Call("do_test_radix_find", x, tbl, x0, PACKAGE = packageName())
}

test_radix_find_range <- function(x, tbl) {
  stopifnot(is.integer(x), length(x) == 1, !is.unsorted(tbl))
  .Call("do_test_radix_find_range", x, tbl, PACKAGE = packageName())
}

sum_in <- function(x, tbl, sorted = FALSE) {
  .Call("n_sin", x, tbl, sorted, PACKAGE = packageName())
}
