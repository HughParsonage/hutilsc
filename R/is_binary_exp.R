#' Is a call a binary expression
#' 
#' @param sexp A call, likely the result of \code{substitute(expr)}
#' within a function on a user-supplied \code{expr}.
#' 
#' @return \code{TRUE} if a binary expression.
#' 
#' @export

is_binary_expr <- function(sexp) {
  .Call("is_binary_call", sexp, PACKAGE = packageName())
}


is_binary_exp <- function(expr, envir = parent.frame()) {
  sexpr <- substitute(expr)
  is.call(sexpr) &&
    length(sexpr) == 3L
}



