# nocov start
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("hutilsc.nThread"))) {
    options("hutilsc.nThread" = 1L)
  }
  invisible(NULL)
}

.onUnload <- function (libpath) {
  library.dynam.unload(packageName(), libpath)
}
# nocov end