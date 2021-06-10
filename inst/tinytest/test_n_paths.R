library(tinytest)
library(hutilsc)
library(data.table)
dist_bw_edges <- hutilsc:::dist_bw_edges
dist_bw_edge_igraph <- hutilsc:::dist_bw_edge_igraph
EdgesNP <- data.table(orig = c(1L, rep(2L, 3), 3L, 4L, 5L, rep(6L, 3)),
                      dest = c(2:6, 6L, 6L, 7:9),
                      key = "orig,dest")
nPaths <- n_paths_between_given_dist(EdgesNP, return_dt = TRUE)
nPaths <- NULL

expect_equal(n_paths_svt(s = 1L,
                         t = 2L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_svt(s = 1L,
                         t = 3L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_svt(s = 2L,
                         t = 6L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             3L)
expect_equal(n_paths_svt(s = 2L,
                         v = 3L,
                         t = 6L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_svt(s = 1L,
                         v = 2L,
                         t = 6L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             3L)

EdgesNP2 <- rbind(EdgesNP, 
                  data.table(orig = c(1L, 3L),
                             dest = c(3L, 9L)),
                  use.names = TRUE)
setkey(EdgesNP2, orig, dest)
expect_equal(n_paths_svt(s = 1L,
                         t = 9L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             3L)
expect_equal(n_paths_svt(s = 1L,
                         v = 5L,
                         t = 9L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)

EdgesNP3 <- rbind(EdgesNP, 
                  data.table(orig = c(1L, 3L),
                             dest = c(3L, 9L)),
                  use.names = TRUE)


EdgesOutCentre <-
  data.table(orig = 1:6,
             dest = 7L,
             key ="orig,dest")

ci <- function(...) as.integer(c(...))
# Triakis Tetrahedral Graph
EdgeTriakis <- data.table(orig = ci(1, 1, 1, 1, 1, 1,
                                    2, 2,
                                    3, 3, 3, 3,
                                    4,
                                    5, 5, 
                                    6, 6, 
                                    7),
                          dest = ci(2, 3, 4, 6, 7, 8,
                                    3, 8,
                                    4, 5, 6, 8,
                                    6, 
                                    6, 8, 
                                    7, 8,
                                    8),
                          key = "orig,dest")

WeinerAraya <-
  fread(text = "orig dest
1 2
1 24
1 35
1 38
2 13
2 40
2 41
13 24
13 3
13 4
24 6
24 7
35 8
35 11
38 39
38 12
39 40
39 15
40 17
41 42
41 18
42 3
42 20
3 22
4 5
4 23
5 6
5 26
6 28
7 8
7 29
8 9
9 10
9 30
10 11
10 31
11 12
12 14
14 15
14 31
14 32
15 16
16 17
16 33
17 18
18 19
19 20
19 33
20 21
21 22
21 34
22 23
23 25
25 26
25 34
25 36
26 27
27 28
27 37
28 29
29 30
30 37
31 37
32 33
32 36
33 34
36 37",
        sep = " ", key = "orig,dest")


SnakeEdgelist <- 
  data.table(orig = ci(1, 1, 
                       2, 
                       3, 3, 
                       4, 
                       5, 5, 
                       6, 
                       7, 7, 
                       8),
             dest = c(2, 3, 
                      3,
                      4, 5, 
                      5,
                      6, 7,
                      7,
                      8, 9,
                      9),
             key = "orig,dest")

hutilsc_distances <- dist_bw_edges(SnakeEdgelist)
igraph__distances <- dist_bw_edge_igraph(SnakeEdgelist)
expect_equal(hutilsc_distances, hutilsc_distances)

DisconnectedSnakes <-
  rbind(SnakeEdgelist, 
        SnakeEdgelist[, lapply(.SD, "+", 10L)], 
        use.names = TRUE)
setkey(DisconnectedSnakes, orig, dest)


Spray <- data.table(orig = ci(1, 1, 1, 
                              2, 3, 4, 
                              5, 5, 5, 
                              6, 7, 8, 
                              9, 9, 9, 
                              10, 11, 12, 
                              13, 13, 13),
                    dest = ci(2, 3, 4, 
                              5, 5, 5, 
                              6, 7, 8, 
                              9, 9, 9, 
                              10, 11, 12, 
                              13, 13, 13,
                              14, 15, 16),
                    key = "orig,dest")
## Test n_paths s-v-t for s < t < v 
RetroEdges <- data.table(orig = c(1L, 1L, 
                                  3L, 
                                  4L, 4L, 4L,
                                  5L, 5L),
                         dest = c(4L, 5L,
                                  2L,
                                  2L, 3L, 6L,
                                  2L, 6L),
                         key = "orig,dest")
expect_equal(n_paths_svt0(s = 1L, t = 2L, Edges = RetroEdges), 2L) # 1--4--2, 1--5--2
expect_equal(n_paths_svt0(s = 2L, t = 1L, Edges = RetroEdges), 2L) # 1--4--2, 1--5--2
expect_equal(n_paths_svt0(s = 1:2, t = 2:1, Edges = RetroEdges), c(2L, 2L)) # 1--4--2, 1--5--2







