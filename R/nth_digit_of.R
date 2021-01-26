

nth_digit_of <- function(x, d) {
  stopifnot(is.integer(x), is.integer(d), length(d) == 1)
  .Call("NthDigit", x, d, PACKAGE = packageName())
}
