
j <- c(4L, 9L, 7L, 6L, 8L, 10L, 2L, 3L, 1L, 5L)
k <- c(7L, 10L, 8L, 6L, 3L, 1L, 4L, 5L, 2L, 9L)

and2s <- hutilsc:::and2s

for (i in 2:9) {
  j <- rep(j, i)
  k <- rep(k, i)
  expect_equal(and2s(j == 1L, k == 2L),
               `&`(j == 1L, k == 2L))
  expect_equal(and2s(j >= 1L, k >= 2L),
               `&`(j >= 1L, k >= 2L))
  expect_equal(and2s(j != 1L, k == 2L),
               `&`(j != 1L, k == 2L))
  expect_equal(and2s(j != 1L, k < 2L),
               `&`(j != 1L, k < 2L))
  JJ <- j > 1L
  expect_equal(and2s(JJ, JJ), JJ)
}

Z <- j == j
expect_equal(and2s(Z, k > -1L), Z)
one_true <- TRUE
expect_equal(and2s(one_true, k >= 5L), k >= 5L)

J <- j >= 5L
K <- k >= 3L
expect_equal(and2s(j >= 5L, k >= 3L), 
             and2s(J, K))
expect_equal(and2s(J, K), J & K)

J <- j > 5L
K <- k > 2L
expect_equal(and2s(j > 5L, k > 2L), 
             and2s(J, K))
expect_equal(and2s(J, K), J & K)

J <- j < 8L
K <- k > 2L
expect_equal(and2s(j < 8L, k > 2L), 
             and2s(J, K))
expect_equal(and2s(J, K), J & K)

J <- j >= 9L
K <- k <= 9L
expect_equal(and2s(j >= 9L, k <= 9L), 
             and2s(J, K))
expect_equal(and2s(J, K), J & K)



