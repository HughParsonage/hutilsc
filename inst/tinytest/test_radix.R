
test_radix_find <- hutilsc:::test_radix_find
test_radix_find_range <- hutilsc:::test_radix_find_range

kilo1 <- c(-1L, 3L, 3L, 3L, 5L, 6L)
expect_equal(hutilsc:::test_radix_find(3L, kilo1), 2L)
expect_equal(hutilsc:::test_radix_find(max(kilo1) + 1L, kilo1), -1L)
expect_equal(hutilsc:::test_radix_find_range(3L, kilo1), c(1L, 3L))

kilo1_large_a <- 1:1e7
expect_equal(hutilsc:::test_radix_find(1e6L, kilo1_large_a), 1e6L)
kilo1_large_b <- rep(-5:100, each = 1001)
expect_equal(hutilsc:::test_radix_find(9L, kilo1_large_b), 
             which.max(kilo1_large_b == 9L))
expect_equal(hutilsc:::test_radix_find_range(9L, kilo1_large_b) + 1L, 
             c(which.max(kilo1_large_b == 9L),
               which.max(kilo1_large_b > 9L) - 1L))
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_seed(11, {
    kilo1_large_c <- sort(rep_len(sample(200) - 20L, 1e7), 
                          na.last = TRUE)
    x <- sample(200, size = 1)
    expect_equal(hutilsc:::test_radix_find(x, kilo1_large_c), 
                 which.max(kilo1_large_c == x))
  })
}

x <- 1:101
expect_equal(diff(sapply(x, test_radix_find, x)),
             rep(1L, 100L))
expect_equal(sapply(x, test_radix_find_range, x), 
             matrix(0:100, ncol = 101, nrow = 2, byrow = TRUE))

x <- 1:100
expect_equal(diff(sapply(x, test_radix_find, x)),
             rep(1L, 99L))
expect_equal(sapply(x, test_radix_find_range, x), 
             matrix(0:99, ncol = 100, nrow = 2, byrow = TRUE))

x <- cumsum(1:37)

# test table lookups
expect_equal(test_radix_find_range(c(2L, 5L, 6L), x, use_tp = TRUE), 
             c(-1L, 0L, -1L, 0L, 2L, 2L))


find_first <- hutilsc:::test_find_first

for (i in seq_len(max(x))) {
  expect_equal(find_first(i, x, x), match(i, x, nomatch = 0L))
  expect_false(xor(hutilsc::find_ftc(i, x, return_logical = TRUE), 
                   i %in% x))
  expect_equal(hutilsc::find_ftc(i, x, return_logical = FALSE), 
               match(i, x, nomatch = 0L))
}







  




