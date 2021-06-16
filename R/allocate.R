

allocate0_int <- function(n, nthread = 1L) {
  .Call("Callocate0_int", n, nthread, PACKAGE = packageName())
}

every_int32 <- function(nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("Cevery_int32", nThread, PACKAGE = packageName())
}
