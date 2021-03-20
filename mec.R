suppressMessages({
  libraries();
  de_in()
})

TriangleEdges <- 
  data.table(x = c(1:17),
             y = c(2,
                   12,
                   2,
                   8, 9, 10, 11,
                   12, 13, 14, 15,
                   13, 16, 16, 17, 18, 18))
TriangleEdges[, y := as.integer(y)]
setkey(TriangleEdges, x, y)
for (o in 4:6) {
  cat("o =", o, "\n")
  ego_net(1L, order = o, TriangleEdges)
}
for (i in 1:100) {
  Edges <- fread("~/dhhs/dhhs-names/Edges.tsv", key = "from,to")
  cat(s <- sample(Edges$from, size = 1))
  ego_net(s, order = 6L, Edges = Edges)
  cat(".")
  ego_net(NULL, order = 6L, Edges = Edges)
}
cat("Complete\n")






