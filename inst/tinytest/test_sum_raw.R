library(hutilsc)
library(tinytest)

x <- raw(5)
expect_equal(sum_raw(x), 0)
x <- c(as.raw(0), as.raw(1), as.raw(0))
expect_equal(sum_raw(x), 1)
expect_equal(sum_raw(charToRaw('a')), utf8ToInt('a'))
out <- integer(256)
out[2] <- 1L
expect_equal(hutilsc:::tabulate_raw(as.raw(1L)), out)

