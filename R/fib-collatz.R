


fibonacci <- function(n) {
  .Call("do_fibonacci", n, TRUE, PACKAGE = packageName())
}

collatz <- function(s) {
  .Call("do_collatz", s, PACKAGE = packageName())
}
