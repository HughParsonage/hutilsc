
.fib <- function(x) {
  if (length(x) <= 1) {
    return(c(1L, 1L))
  }
  c(x[1] + x[2], x)
}

fib <- function(n) {
  o <- .fib(NULL)
  while(length(o) <= n) {
    o <- .fib(o)
  }
  rev(o[-1])
}

expect_equal(hutilsc:::fibonacci(7L), 
             fib(7L))
