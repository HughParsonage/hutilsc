
sum_pack <- function(x) {
  .Call("CpackSum", x, PACKAGE = packageName())
}
