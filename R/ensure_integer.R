

ensure_integer <- function(x) {
  if (is.integer(x)) {
    return(x)
  }
  if (is.double(x)) {
    # return(.Call("do_ensureInteger_dbl", x, PACKAGE = packageName()))
  }
}


