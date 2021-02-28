library(hutilsc)
unique_sorted <- hutilsc:::unique_sorted

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

x <- c(logical(10), TRUE, NA)
csl <- hutilsc:::counting_sort_logi(x)
expect_equal(csl, x[order(x)])

x <- c(1L, 3L, 3L, 5L)
expect_identical(unique_sorted(x), unique(x))



