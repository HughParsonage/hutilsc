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
  Edges <- fread("~/dhhs/dhhs-names/Edges.tsv", key = "orig,dest")
  cat(s <- sample(Edges$orig, size = 1))
  ego_net(s, order = 6L, Edges = Edges)
  cat(".")
  ego_net(NULL, order = 6L, Edges = Edges)
}
u <- Edges[, union(orig, dest)]
bench::system_time(sapply(u, ego_net))

Edges_E <- fread("~/dhhs/dhhs-names/egoFail-2021-03-23.tsv", key = "orig,dest")
# 25442979
Cego <- ego_net(21L, order = 2L, Edges = Edges_E)
Bego <- basic_ego_net(21L, order = 2L, Edges = Edges_E)



cat("Complete\n")






