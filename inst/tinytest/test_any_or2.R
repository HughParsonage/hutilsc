
library(data.table)

test_2expr <- function(irow,
                       n1, n2, op1, op2, m1, m2, 
                       type1 = "integer", 
                       type2 = "integer",
                       seed = 1,
                       orig_seed = get0(".Random.seed")) {
  stopifnot(type1 %in% c("integer", "double", "character"))
  stopifnot(type2 %in% c("integer", "double", "character"))
  force(irow)
  set.seed(seed)
  on.exit(set.seed(orig_seed))
  x1 <- sample(-5:10, size = n1, replace = TRUE)
  y1 <- sample(-5:10, size = m1, replace = TRUE)
  x2 <- sample(-5:10, size = n2, replace = TRUE)
  y2 <- sample(-5:10, size = m2, replace = TRUE)
  ops <- c("!=", "==", ">=", "<=", ">", "<", "%in%")
  
  if (type1 == "double") {
    x1 <- as.double(x1)
  }
  if (type2 == "double") {
    x2 <- as.double(x2)
  }
  
  ANS <- 
    eval(parse(text = paste("any_or2(x1", ops[op1], "y1,", "x2", ops[op2], "y2);")))
  
  EXP <-
    eval(parse(text = paste("any(`|`(x1", ops[op1], "y1,", "x2", ops[op2], "y2), na.rm = TRUE)")))
  
  identical(ANS, EXP)
}

Cj <- 
  CJ(n1 = c(1L, 101L), 
     n2 = c(1L, 101L),
     op1 = 1:7,
     op2 = 1:7,
     m1 = c(1L, 101L),
     m2 = c(1L, 101L),
     type1 = c("integer", "double"),
     type2 = c("integer", "double"))
Cj[, ii := .I]
Cj[, res := test_2expr(.BY[[1]], n1, n2, op1, op2, m1, m2, type1, type2), by = "ii"]
for (r in 1:nrow(Cj)) {
  expect_true(Cj$res[r], info = r)
}


