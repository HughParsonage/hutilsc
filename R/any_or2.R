


any_or2 <- function(expr1, expr2,
                    nThread = getOption("hutilsc.nThread", 1L)) {
  sexpr1 <- substitute(expr1)
  sexpr2 <- substitute(expr2)
  if (!is.call(sexpr1) || length(sexpr1) != 3L ||
      !is.call(sexpr2) || length(sexpr2) != 3L) {
    return(any(expr1 | expr2, na.rm = TRUE))
  }
  cexpr1 <- as.character(sexpr1)
  cexpr2 <- as.character(sexpr2)
  o1 <- op2M(cexpr1[[1L]])
  o2 <- op2M(cexpr2[[1L]])
  x1 <- eval.parent(sexpr1[[2L]])
  x2 <- eval.parent(sexpr2[[2L]])
  y1 <- eval.parent(sexpr1[[3L]])
  y2 <- eval.parent(sexpr2[[3L]])
  
  ans <- .Call("Cany_or2", 
               x1, o1, y1, 
               x2, o2, y2, 
               nThread, 
               PACKAGE = packageName())
  if (is.null(ans)) {
    # Some failure
    return(any(expr1 | expr2, na.rm = TRUE))
  }
  ans
}




