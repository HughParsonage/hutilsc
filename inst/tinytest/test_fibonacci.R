
.fib <- function(x) {
  if (length(x) <= 1) {
    return(c(1L, 1L))
  }
  # sum not + for double
  c(sum(x[1], x[2]), x)
}

fib <- function(n) {
  o <- .fib(NULL)
  while(length(o) <= n) {
    o <- .fib(o)
  }
  rev(o[-1])
}


for (i in 1:50) {
expect_equal(hutilsc:::fibonacci(i), 
             fib(i), 
             info = paste0("i = ", i))
}
