expect_equal(ensure_fwc(letters), letters)
expect_equal(ensure_fwc(c("ABC", "1")), c("ABC", "001"))

