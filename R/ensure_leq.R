

ensure_leq <- function(k1, k2) {
  if (.Call("Censure_leq", k1, k2, PACKAGE = packageName())) {
    # Handled
    return(invisible(list(k1, k2))) 
  }
  
  invisible(list(pmin(k1, k2), 
                 pmax(k1, k2)))
  
}

enseq <- function(x, use = TRUE, zero_based = FALSE) {
  o <- match(x, x[order(x)])
  if (isTRUE(zero_based)) {
    return(o - 1L)
  }
  o
}

hrorder <- function(x, o, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("Crorder", x, o, nThread, PACKAGE = packageName())
}


