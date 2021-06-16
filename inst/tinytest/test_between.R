library(hutilsc)
library(tinytest)
library(data.table)

if (Sys.getenv("USERNAME") == "hughp" || 
    !requireNamespace("hutilscpp", quietly = TRUE)) {
  gc(full = TRUE, verbose = FALSE)
  expect_true(TRUE)
  x <- pcg_hash(4e9, r = 1:50, nThread = 8L)
  actual <- Between(x, -2e9L, 2e9L)
  expect <- and2s(x >= -2e9L, x <= 2e9L, nThread = 10L)
  expect_identical(actual, expect)
  x <- actual <- expect <- NULL
  x <- every_int32(10L)
  actual <- Between(x, -2e9L, 2e9L)
  expect <- and2s(x >= -2e9L, x <= 2e9L, nThread = 10L)
  expect_identical(actual, expect)
  for (j in c(0L, 1L, 2L, 15L, 31L)) {
    for (k in c(0L, 1L, 2L, 15L, 31L)) {
      expect_true(is.integer(j))
      actual <- expect <- NULL
      lwr <- -as.integer(2^j - 1)
      upr <- as.integer(2^j - 1)
      actual <- Between(x, lwr, upr)
      expect <- and2s(x >= lwr, x <= upr, nThread = 10L)
      expect_identical(actual, expect)
    }
  }
} else {
  
}

