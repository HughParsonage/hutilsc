
kilo1 <- c(-1L, 3L, 3L, 3L, 5L, 6L)
expect_equal(hutilsc:::test_radix_find(3L, kilo1), 1L)
expect_equal(hutilsc:::test_radix_find(max(kilo1) + 1L, kilo1), length(kilo1) - 1L)
expect_equal(hutilsc:::test_radix_find_range(3L, kilo1), c(1L, 3L))

kilo1_large_a <- 1:1e7
expect_equal(hutilsc:::test_radix_find(1e6L, kilo1_large_a), 1e6L - 1L)
kilo1_large_b <- rep(-5:100, each = 1001)
expect_equal(hutilsc:::test_radix_find(9L, kilo1_large_b), 
             which.max(kilo1_large_b == 9L) - 1L)
expect_equal(hutilsc:::test_radix_find_range(9L, kilo1_large_b) + 1L, 
             c(which.max(kilo1_large_b == 9L),
               which.max(kilo1_large_b > 9L) - 1L))
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_seed(11, {
    kilo1_large_c <- sort(rep_len(sample(200) - 20L, 1e7), 
                          na.last = TRUE)
    x <- sample(200, size = 1)
    expect_equal(hutilsc:::test_radix_find(x, kilo1_large_c), 
                 which.max(kilo1_large_c == x) - 1L)
  })
}

