
Between <- function(x, a, b, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_between", x, a, b, nThread, PACKAGE = "hutilsc")
}

