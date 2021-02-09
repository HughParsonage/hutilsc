
j <- c(4L, 9L, 7L, 6L, 8L, 10L, 2L, 3L, 1L, 5L)
k <- c(7L, 10L, 8L, 6L, 3L, 1L, 4L, 5L, 2L, 9L)

and2s <- hutilsc:::and2s

expect_equal(and2s(j == 1L, k == 2L),
             `&`(j == 1L, k == 2L))

Z <- j == j
expect_equal(and2s(Z, k > -1L), Z)
one_true <- TRUE
expect_equal(and2s(one_true, k >= 5L), k >= 5L)


