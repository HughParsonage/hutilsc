

allocate0_int <- function(n, nthread = 1L) {
  .Call("Callocate0_int", n, nthread, PACKAGE = packageName())
}