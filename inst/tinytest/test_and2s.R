
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

test_2expr <- function(irow,
                       n1, n2, op1, op2, m1, m2, 
                       type1 = "integer", 
                       type2 = "integer",
                       seed = 1,
                       orig_seed = get0(".Random.seed")) {
  force(irow)
  set.seed(seed)
  on.exit(set.seed(orig_seed))
  x1 <- sample(-5:10, size = n1, replace = TRUE)
  y1 <- sample(-5:10, size = m1, replace = TRUE)
  x2 <- sample(-5:10, size = n2, replace = TRUE)
  y2 <- sample(-5:10, size = m2, replace = TRUE)
  ops <- c("!=", "==", ">=", "<=", ">", "<", "%in%")
  
  ANS <- 
    eval(parse(text = paste("and2s(x1", ops[op1], "y1,", "x2", ops[op2], "y2);")))
  
  EXP <-
    eval(parse(text = paste("`&`(x1", ops[op1], "y1,", "x2", ops[op2], "y2);")))
  
  identical(ANS, EXP)
}

library(data.table)
Cj <- 
  CJ(n1 = c(1L, 101L), 
     n2 = c(1L, 101L),
     op1 = 1:7,
     op2 = 1:7,
     m1 = c(1L, 101L),
     m2 = c(1L, 101L))
Cj[, ii := .I]
Cj[, res := test_2expr(.BY[[1]], n1, n2, op1, op2, m1, m2), by = "ii"]
for (r in 1:nrow(Cj)) {
  expect_true(Cj$res[r], info = r)
}












