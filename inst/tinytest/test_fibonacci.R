
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


if (hutilsc:::is64bit()) {
  for (i in 1:50) {
    res <- hutilsc:::fibonacci(i)
    exp <- fib(i)
    expect_equal(res,
                 exp,
                 info = paste0("i = ", i, " :: ",
                               "h(res) = ", head(res), ",",
                               "t(res) = ", tail(res), ";",
                               "h(exp) = ", head(exp), ",",
                               "t(exp) = ", tail(exp), ""))
  }
}