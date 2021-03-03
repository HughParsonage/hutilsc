
library(data.table)
any_or2 <- hutilsc:::any_or2

# Two parts: Known failures and Entire combination
## Known failures
x <- 1:10
xd <- x + 0
expect_true(any_or2(x == 1.5, x == 1))
expect_true(any_or2(x == 1.5, xd == 1))
expect_false(any_or2(x == 1.5, x == 1.5))
expect_false(any_or2(x == 1.5, xd == 1.5))

## Entire combination (Cj)
test_2expr <- function(irow,
                       n1, n2, op1, op2, m1, m2, 
                       typen1 = "integer", 
                       typen2 = "integer",
                       typem1 = "integer",
                       typem2 = "integer",
                       seed = get0("seed_"),
                       orig_seed = get0(".Random.seed")) {
  stopifnot(typen1 %in% c("integer", "double", "character"))
  stopifnot(typen2 %in% c("integer", "double", "character"))
  force(irow)
  set.seed(seed)
  on.exit(set.seed(orig_seed))
  x1 <- sample(-5:10, size = n1, replace = TRUE)
  y1 <- sample(-5:10, size = m1, replace = TRUE)
  x2 <- sample(-5:10, size = n2, replace = TRUE)
  y2 <- sample(-5:10, size = m2, replace = TRUE)
  ops <- c("!=", "==", ">=", "<=", ">", "<", "%in%")
  
  if (typen1 == "double") {
    x1 <- as.double(x1)
    if (n1 > 2) {
      x1[1:2] <- c(0.5, -0.5)
    }
  }
  if (typen2 == "double") {
    x2 <- as.double(x2)
  }
  if (typem1 == "double") {
    y1 <- as.double(y1)
  }
  if (typem2 == "double") {
    y2 <- as.double(y2)
  }
  
  ANS <- 
    eval(parse(text = paste("any_or2(x1", ops[op1], "y1,", "x2", ops[op2], "y2);")))
  
  EXP <-
    eval(parse(text = paste("any(`|`(x1", ops[op1], "y1,", "x2", ops[op2], "y2), na.rm = TRUE)")))
  
  identical(ANS, EXP)
}

seed_ <- 1L

Cj <- 
  CJ(n1 = c(1L, 101L), 
     n2 = c(1L, 101L),
     op1 = 1:7,
     op2 = 1:7,
     m1 = c(1L, 101L),
     m2 = c(1L, 101L),
     typen1 = c("integer", "double"),
     typem1 = c("integer", "double"),
     typen2 = c("integer", "double"),
     typem2 = c("integer", "double"))
Cj[, ii := .I]
setkey(Cj, ii)
Cj[, res := test_2expr(.BY[[1]], n1, n2, op1, op2, m1, m2, typen1, typen2, typem1, typem2), by = "ii"]
for (r in 1:nrow(Cj)) {
  expect_true(Cj$res[r], info = r)
}

seed_ <- 2L

Cj[, res := test_2expr(.BY[[1]], n1, n2, op1, op2, m1, m2, typen1, typen2, typem1, typem2), by = "ii"]
for (r in 1:nrow(Cj)) {
  expect_true(Cj$res[r], info = r)
}


