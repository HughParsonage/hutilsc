

Strlen <- function(x, m = 0L) {
  .Call("do_Strlen", x, m, PACKAGE = packageName())
}

