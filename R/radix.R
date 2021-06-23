

test_radix_find <- function(x, tbl, x0 = 0L, check_sorted = TRUE, use_tp = FALSE) {
  stopifnot(is.integer(x), length(x) == 1L, is.integer(tbl))
  if (isTRUE(check_sorted)) {
    stopifnot(!is.unsorted(tbl))
  }
  .Call("Ctest_radix_find", x, tbl, x0, PACKAGE = packageName())
}

test_radix_find_range <- function(x, tbl, check_sorted = TRUE, use_tp = FALSE) {
  stopifnot(is.integer(x), is.integer(tbl))
  if (length(x) != 1 && !isTRUE(use_tp)) {
    stop("length(x) != 1 && !isTRUE(use_tp)") # nocov
  }
  if (isTRUE(check_sorted)) {
    stopifnot(!is.unsorted(tbl))
  }
  .Call("Ctest_radix_find_range", x, tbl, isTRUE(use_tp), PACKAGE = packageName())
}

find_ftc <- function(x, tbl, return_logical = TRUE, nThread = 10L) {
  .Call("Cfind_ftc", x, tbl, nThread, isTRUE(return_logical), PACKAGE = packageName())
}

test_find_first <- function(x, k1, k2, u = NULL) {
  if (is.null(u)) {
    u <- union(k1, k2)
    u <- u[order(u)]
  }
  stopifnot(is.integer(x), is.integer(k1), is.integer(u)) 
  .Call("Ctest_find_first", x, k1, u, PACKAGE = packageName())
}


bsearch <- function(a, x) {
  .Call("do_bsearch", a, x, PACKAGE = packageName())
}

