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

op2M <- function(operator) {
  switch(operator,
         `!=` = 1L,
         `==` = 2L,
         `>=` = 3L, 
         `<=` = 4L,
         `>` = 5L,
         `<` = 6L,
         `%in%` = 7L, 
         `%between%` = 8L,
         `%(between)%` = 9L,
         `%]between[%` = 10L,
         0L)
}
