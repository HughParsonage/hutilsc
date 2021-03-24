


fibonacci <- function(n) {
  .Call("Cfibonacci", n, TRUE, PACKAGE = packageName())
}

collatz <- function(s) {
  .Call("Ccollatz", s, PACKAGE = packageName())
}
