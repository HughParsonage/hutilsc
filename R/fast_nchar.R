

fast_nchar <- function(x) {
  .Call("Cfast_nchar", x, PACKAGE = packageName())
}

