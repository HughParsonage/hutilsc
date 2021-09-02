
s <- sample(1003, size = 1002)
expect_identical(findAbsent(s), setdiff(1:1003, s))
