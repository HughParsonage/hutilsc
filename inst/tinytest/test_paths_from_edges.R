

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

Edges <- data.table(x = 1:10, y = 2:11, key = "x,y")

DT <- data.table(x = c(1L, 1L, 2L, 4L, 8L, 9L),
                 y = c(2L, 3L, 4L, 5L, 9L, 10L))
setkey(DT, x, y)
color_subgraphs(DT)
expect_equal(DT$color, c(1L, 1L, 1L, 1L, 2L, 2L))

DT2 <- data.table(x = c(8L, 9L, 12L, 13L, 16L),
                  y = c(16L, 16L, 13L, 14L, 17L))
setkey(DT2, x, y)
color_subgraphs(DT2, new_col = "color2")
expect_equal(DT2$color2, c(1L, 2L, 3L, 3L, 2L))
if (FALSE) {
# Multiple paths
DT <- data.table(x = c(1L, 1L, 1L, 2L, 3L, 4L, 5L), 
                 y = c(2L, 3L, 4L, 5L, 5L, 5L, 6L),
                 key = "x,y")
path_a <- paths_from_edges(1L, 5L, Edges = DT)
}



