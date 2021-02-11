
sum_pack <- function(x) {
  .Call("do_packSum", x, PACKAGE = packageName())
}
