

fuse1 <- function(x, k1, k2) {
  .Call("do_fuse1", x, k1, k2, PACKAGE = packageName())
}

