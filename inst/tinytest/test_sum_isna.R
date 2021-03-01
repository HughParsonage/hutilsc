x <- 1:10
expect_equal(sum_isna(x), 0L)
x <- c(NA, x)
expect_equal(sum_isna(x), 1L)
