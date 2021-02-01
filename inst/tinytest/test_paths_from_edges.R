
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


