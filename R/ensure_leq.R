

ensure_leq <- function(k1, k2) {
  if (.Call("Censure_leq", k1, k2, PACKAGE = packageName())) {
    # Handled
    return(invisible(list(k1, k2))) 
  }
  
  invisible(list(pmin(k1, k2), 
                 pmax(k1, k2)))
  
}

enseq <- function(x, use = TRUE) {
  if (use) {
    .Call("Censeq", x, PACKAGE = packageName())
  } else {
    match(x, unique(sort(x) - min(x) + 1L))
  }
}
