
sum_pack <- function(x, m = 0L) {
  .Call("CpackSum", x, m, PACKAGE = packageName())
}

sum_int <- function(x) {
  .Call("Csum_int", x, PACKAGE = "hutilsc")
}
