
library(data.table)
TriangleEdges <- 
  data.table(x = c(1:17),
             y = c(2,
                   12,
                   2,
                   8, 9, 10, 11,
                   12, 13, 14, 15,
                   13, 16, 16, 17, 18, 18))
TriangleEdges[, y := as.integer(y)]

AlmostTriangle <-
  rbind(TriangleEdges, 
        data.table(x = 17L, y = 19L),
        data.table(x = 100:105, y = 101:106))
setkey(AlmostTriangle, x, y)


