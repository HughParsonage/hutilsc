library(hutilsc)

x <- 1:5
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- 1:5 + 0L
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- integer(5)
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- c(10L, 1:5)
expect_false(is_sorted(x))
expect_equal(isntSorted(x), 1L)


x <- as.double(1:5)
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- 1:5 + 0
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- double(5)
expect_true(is_sorted(x))
expect_equal(isntSorted(x), 0L)

x <- c(10, 1:5)
expect_false(is_sorted(x))
expect_equal(isntSorted(x), 1L)

expect_true(is_sorted(as.character(1:5)))
expect_false(is_sorted(as.character(c(1:5, 5:1))))



