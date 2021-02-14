

fuse1 <- function(x, k1, k2) {
  .Call("do_fuse1", x, k1, k2, PACKAGE = packageName())
}

ensure_leq <- function(k1, k2) {
  .Call("do_ensure_leq", k1, k2)
  invisible(list(k1, k2))
}
