
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

expect_true(any_or2(x == -1L, xd > 9L))

x_single <- 1L
y_single <- 1L
expect_true(any_or2(x_single == y_single, x > 10L))
expect_false(any_or2(x_single != y_single, x > 10L))
xd_single <- 1
yd_single <- 1
expect_true(any_or2(xd_single == yd_single, x > 10L))
expect_true(any_or2(xd_single == y_single, x > 10L))
expect_false(any_or2(xd_single < yd_single, x < x))

theEnv <- new.env(parent = baseenv())
assign("X1", c(0L, -5L, -2L, -1L, 3L, 6L, 2L, 7L, 8L, 1L, 4L, 9L, 5L, -3L), envir = theEnv)
assign("X2", c(5L, 9L, 5L, 4L, 3L, 0L, 0L, 1L, 1L, -3L, 9L, 9L, 8L, -3L), envir = theEnv)
assign("Y1", c(6L, 2L, 2L, -1L, 8L, 3L, 3L, 6L, 0L, 0L, 0L, 1L, 5L, 12L), envir = theEnv)
assign("Y2", c(8L, 7L, 9L, 9L, 1L, 10L, 13L, 10L, 2L, 4L, 3L, 10L, 6L, 12L), envir = theEnv)

## Entire combination (Cj)
test_2expr <- function(irow,
                       n1, n2, op1, op2, m1, m2, 
                       typen1 = "integer", 
                       typen2 = "integer",
                       typem1 = "integer",
                       typem2 = "integer",
                       Env = theEnv,
                       seed = get0("seed_"),
                       orig_seed = get0(".Random.seed")) {
  stopifnot(typen1 %in% c("integer", "double", "character"))
  stopifnot(typen2 %in% c("integer", "double", "character"))
  force(irow)
  ops <- c("!=", "==", ">=", "<=", ">", "<", "%in%")
  
  x1 <- get("X1", envir = Env)
  x2 <- get("X2", envir = Env)
  y1 <- get("Y1", envir = Env)
  y2 <- get("Y2", envir = Env)
  
  if (n1 == 1) {
    x1 <- x1[1]
  }
  if (n2 == 1) {
    x2 <- x2[1]
  }
  if (m1 == 1) {
    y1 <- y1[1]
  }
  if (m2 == 1) {
    y2 <- y2[1]
  }
  
  
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

faire_or2_expr_out <- function(tx1 = "integer", tx2 = "integer", ty1 = "integer", ty2 = "integer",
                               op1 = "!=", 
                               op2 = "!=",
                               lx1 = 1L, lx2 = 1L, ly1 = 1L, ly2 = 1L, 
                               outcome1 = FALSE,
                               outcome2 = FALSE) {
  base_any_or2 <- function(expr1, expr2) any(expr1 | expr2)
  
  
  
  x1 <- seq_len(lx1) + switch(tx1, integer = 0L, double = 0)
  x2 <- -seq_len(lx2) + switch(tx2, integer = 0L, double = 0)
  
  y1 <- integer(ly1) + switch(ty1, integer = 1L, double = 1)
  y2 <- integer(ly2) + switch(ty2, integer = 1L, double = 1)
  
  EXP <- eval(parse(text = paste0("base_any_or2(x1 ", op1, " y1, x2 ", op2, " y2)")))
  ACT <- eval(parse(text = paste0("any_or2(x1 ", op1, " y1, x2 ", op2, " y2)")))
  identical(EXP, ACT)
}

CJ2 <- CJ(tx1 = c("integer", "double"),
          ty1 = c("integer", "double"),
          tx2 = c("integer", "double"),
          ty2 = c("integer", "double"),
          op1 = c("!=", "==", ">=", "<=", ">", "<", "%in%"),
          op2 = c("!=", "==", ">=", "<=", ">", "<", "%in%"),
          lx1 = 10L,
          lx2 = c(1L, 10L), 
          ly1 = c(1L, 10L), 
          ly2 = c(1L, 10L), 
          sorted = FALSE)
CJ2[, ii := .I]
CJ2[, res := faire_or2_expr_out(tx1, tx2, ty1, ty2, 
                                op1, op2,
                                lx1, lx2, ly1, ly2), 
    by = "ii"]
for (i in 1:nrow(CJ2)) {
  expect_true(CJ2$res[i])
}



