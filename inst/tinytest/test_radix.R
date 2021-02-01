
kilo1 <- c(-1L, 3L, 3L, 3L, 5L, 6L)
expect_equal(hutilsc:::test_radix_find(3L, kilo1), 1L)
expect_equal(hutilsc:::test_radix_find_range(3L, kilo1), c(1L, 3L))

