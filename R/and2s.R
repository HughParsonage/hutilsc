

and2s <- function(exp1, exp2, nThread = 1L) {
  sexp1 <- substitute(exp1)
  sexp2 <- substitute(exp2)
  ans <- NULL
  if (is.symbol(sexp1)) {
    if (length(sexp2) == 3) {
      op2c <- as.character(sexp2[[1L]]) 
      x2 <- eval.parent(sexp2[[2L]])
      y2 <- eval.parent(sexp2[[3L]])
      ans <- .Call("do_and_lgl_int", exp1, x2, op2c, y2, nThread, PACKAGE = packageName())
    }
  } else if (is.call(sexp1) && is.call(sexp1)) {
    if (length(sexp1) == 3L && length(sexp2) == 3L) {
      # (x1 <op1> y1) & (x2 <op2> y2)
      
      op1 <- as.character(sexp1[[1L]])
      op2 <- as.character(sexp2[[1L]])
      x1 <- eval.parent(sexp1[[2L]])
      y1 <- eval.parent(sexp1[[3L]])
      x2 <- eval.parent(sexp2[[2L]])
      y2 <- eval.parent(sexp2[[3L]])
      cat("\n\n", x1, op1, y1, "\n", x2, op2, y2, "\n\n")
      ans <-
        .Call("do_and2s", 
              x1, op1, y1,
              x2, op2, y2, 
              NULL, NULL,
              nThread)
    }
  }
  
  if (is.null(ans)) {
    return(exp1 & exp2)
  } else {
    return(ans)
  }
  
}




