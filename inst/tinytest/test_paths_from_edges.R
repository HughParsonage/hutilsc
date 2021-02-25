
options(hutilsc.verbose = FALSE)


library(data.table)
..Edges <- function() {
  ci <- function(...) as.integer(c(...))
  
  out <- data.table(x = ci(1, 1,
                           2, 2, 2,
                           4, 
                           5, 5, 
                           7),
                    y = ci(2, 3, 
                           3, 4, 5, 
                           5, 
                           6, 7, 
                           10)) 
  setkeyv(out, c("x", "y"))
  out[]
}

## Test ensure_leq
ensure_leq <- hutilsc:::ensure_leq
x <- c(1:5, 6:3)
y <- c(2:5, 1:5)
expect_identical(ensure_leq(x, y),
                 list(pmin(x, y), pmax(x, y)))
expect_error(ensure_leq(1:5, 1.5:5),
             pattern = "typeof")
expect_error(ensure_leq(1:5, 1:6), 
             pattern = "lengths")
x <- c(1:5, 6:3)
y <- c(2:5, 1:5)
xd <- c(1:5, 6:3) + 0
yd <- c(2:5, 1:5) + 0
expect_equal(ensure_leq(as.double(x), as.double(y)), 
             list(pmin(xd, yd), pmax(xd, yd)))

Edges <- data.table(x = 1:10, y = 2:11, key = "x,y")

DT <- data.table(x = c(1L, 1L, 2L, 4L, 8L, 9L),
                 y = c(2L, 3L, 4L, 5L, 9L, 10L))
setkey(DT, x, y)
color_subgraphs(DT)
expect_equal(DT$color, c(1L, 1L, 1L, 1L, 2L, 2L), info = paste0("DT$color = ", toString(DT$color)))

DT2 <- data.table(x = c(8L, 9L, 12L, 13L, 16L),
                  y = c(16L, 16L, 13L, 14L, 17L))
setkey(DT2, x, y)
color_subgraphs(DT2, new_col = "color2")
# expect_equal(DT2$color2, c(1L, 2L, 3L, 3L, 2L))

expect_true(is_valid_path(c(1L, 2L), DT))
expect_true(is_valid_path(c(1L, 3L), DT))
expect_true(is_valid_path(c(1L, 2L, 4L, 5L), DT))
expect_false(is_valid_path(c(1L, 2L, 4L, 9L, 10L), DT))

if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_seed(58, {
    k1 <- sort(c(sample.int(100, size = 500, replace = TRUE), rpois(100, 20)))
    k2 <- k1 + sample(k1)
  })
  VD <- unique(data.table(k1, k2))
  setkey(VD, k1, k2)
  expect_true(is_valid_path(c(20L, 46L, 57L, 80L), VD))
  expect_false(is_valid_path(c(20L, 46L, 78L), VD))
}

# Common contacts
EdgesCC <- 
  rbind(data.table(x = 1L, y = 2:11),
        data.table(x = 2:11, y = 13L))
setkey(EdgesCC, x, y)

expect_equal(common_contacts(1L, 13L, EdgesCC, len = 3L),
             2:11)

EdgesCD <- 
  rbind(data.table(x = 1L, y = 2:11),
        data.table(x = 2:11, y = c(13L, 15L)))
setkey(EdgesCD, x, y)
expect_equal(common_contacts(1L, 13L, EdgesCD, len = 3L), 
             seq.int(2L, 10L, by = 2L))

DifficultToColor <- 
  data.table(x = c(1L, 2L, 2L, 3L, 7L), 
             y = c(7L, 3L, 9L, 4L, 9L))

DifficultToColor <- DifficultToColor[, lapply(.SD, function(x) match(x, sort(unique(unlist(.SD)))))]

setkey(DifficultToColor, x, y)
CliquesD2C <- color_clique(DifficultToColor)
expect_true(is_constant(CliquesD2C[[2]]))

## Len-n paths
Ring <- data.table(orig = c(1:100, 1L), dest = c(2:101, 101L))
setkey(Ring, orig, dest)
L4_Ring101 <- len_four_paths(Ring)

# 1,2,3,4 certainly exists
expect_equal(nrow(L4_Ring101[.(1L, 2L, 3L, 4L), nomatch = 0L]), 1L)

# Test both ensuring inputs are integer (where possible)
# and absence of paths
BiPar <- data.table(from = c(1, 3, 5, 7, 9), to = c(2, 4, 6, 8, 10), key = "from,to")
expect_equal(length(len_three_paths(BiPar, FALSE)), 0)





