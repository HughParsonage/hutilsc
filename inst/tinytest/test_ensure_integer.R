library(hutilsc)

expect_true(is.integer(ensure_integer(1:5)))
expect_true(is.integer(ensure_integer(1)))
expect_true(is.integer(ensure_integer(c(1, 2))))
expect_true(is.integer(ensure_integer(c(1, NA))))  # ok
expect_warning(ensure_integer(c(1, 1.5), on_failure = "warn"), 
               pattern = "integerish.*2")
expect_error(ensure_integer(c(1, 2, 1.5), on_failure = "error"), 
             pattern = "integerish.*3")
expect_error(ensure_integer(c(1, 2, .Machine$integer.max + 1), on_failure = "error"), 
             pattern = "integerish.*3")
expect_error(ensure_integer(c(1, 2, -1 - .Machine$integer.max), on_failure = "error"), 
             pattern = "integerish.*3")
expect_error(ensure_integer(c(1, 2, Inf), on_failure = "error"), 
             pattern = "integerish.*3")


