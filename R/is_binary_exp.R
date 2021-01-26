


is_binary_exp <- function(expr, envir = parent.frame()) {
  sexpr <- substitute(expr, envir = envir)
  is.call(sexpr) &&
    length(sexpr) == 3L
}
