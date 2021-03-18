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
Edges <- fread("~/dhhs/dhhs-names/Edges.tsv", key = "from,to")
ego_net(1L, order = 4L, Edges = Edges)





