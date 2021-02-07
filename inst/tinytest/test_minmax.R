
x <- c(5L, 2L, 1L, 99L)
y <- as.double(x)
z <- as.character(x)

expect_equal(minmax(x), c(min(x), max(x)))
expect_equal(minmax(y), c(min(y), max(y)))
expect_equal(minmax(z), c(min(z), max(z)))
expect_identical(minmax(NULL), NULL)
expect_identical(minmax(integer(0), empty_result = c(Inf, -Inf)), c(Inf, -Inf))
expect_identical(minmax(new.env()), NULL)


expect_equal(wminmax(x), c(which.min(x), which.max(x)))
expect_equal(wminmax(y), c(which.min(y), which.max(y)))
expect_equal(wminmax(z), c(which.min(z), which.max(z)))




