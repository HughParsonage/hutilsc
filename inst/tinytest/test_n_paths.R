library(hutilsc)
library(data.table)
EdgesNP <- data.table(orig = c(1L, rep(2L, 3), 3L, 4L, 5L, rep(6L, 3)),
                      dest = c(2:6, 6L, 6L, 7:9),
                      key = "orig,dest")
nPaths <- n_paths_between_given_dist(EdgesNP, return_dt = TRUE)
nPaths <- NULL

expect_equal(n_paths_stv(s = 1L,
                         t = 2L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_stv(s = 1L,
                         t = 3L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_stv(s = 2L,
                         t = 6L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             3L)
expect_equal(n_paths_stv(s = 2L,
                         v = 3L,
                         t = 6L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             1L)
expect_equal(n_paths_stv(s = 1L,
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
expect_equal(n_paths_stv(s = 1L,
                         t = 9L, 
                         Edges = EdgesNP, 
                         nPaths = nPaths),
             3L)
expect_equal(n_paths_stv(s = 1L,
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



