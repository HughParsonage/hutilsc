
sum_pack <- function(x, m = 0L) {
  .Call("CpackSum", x, m, PACKAGE = packageName())
}
