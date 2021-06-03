x <- as.integer(c(0L, 10^(0:9), 10^(0:9) - 1, NA))
expect_equal(hutilsc_nchar(x), nchar(x))


