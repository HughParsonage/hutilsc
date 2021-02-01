
x <- c(5L, 2L, 1L, 99L)
y <- as.double(x)
z <- as.character(x)

expect_equal(minmax(x), c(min(x), max(x)))
expect_equal(minmax(y), c(min(y), max(y)))
expect_equal(minmax(z), c(min(z), max(z)))


