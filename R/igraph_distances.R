

igraph_distances <- function(Graph) {
  stopifnot(inherits(Graph, "igraph"))
  if (length(igraph::V(Graph)) >= 0.99 * sqrt(.Machine$integer.max)) {
    stop("Graph has ", length(igraph::V(Graph)),  " vertices which will cause a segfault in igraph_distances.")
  }
  igraph::distances(Graph)
}

