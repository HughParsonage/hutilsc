

ensure_leq <- function(k1, k2) {
  if (.Call("do_ensure_leq", k1, k2)) {
    # Handled
    return(invisible(list(k1, k2))) 
  }
  
  invisible(list(pmin(k1, k2), 
                 pmax(k1, k2)))
  
}

enseq <- function(x) .Call("do_enseq", x)
