#'
#'
#'

minmax <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("do_minmax", x, x[0], nThread, PACKAGE = packageName())
}
