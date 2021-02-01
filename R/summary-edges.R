#' Edge summaries
#' 
#' @param a,b Nodes 
#' @param Edges A data.table, keyed, of undirected edges.
#' 
#' 
#' @export

test_reaches_dest <- function(a, b, Edges) {
  stopifnot(is.data.table(Edges), 
            identical(key(Edges)[1:2], names(Edges)[1:2]),
            is.integer(a), 
            is.integer(b),
            length(a) == 1,
            length(b) == 1)
  
  .Call("do_test_reaches_dest", a, b, Edges[[1]], Edges[[2]], PACKAGE = "hutilsc")
}


# profvis::profvis({
reaches_b <- function(b0, b, oEdges) {

  Edges <- oEdges[.(b0), nomatch = 0L]
  print(Edges)
  if (b %in% Edges[[2]]) {
    return(TRUE)
  }
  
  if (nrow(Edges) == 0) {
    cat(".")
    return(FALSE)
  }
  Edges2 <- oEdges[oEdges[.(b0)]]
  any(vapply(Edges2[["dest"]], function(bb) reaches_b(bb, b, oEdges), FUN.VALUE = TRUE))
}

len_b <- function(b0, b, oEdges, max_n = nrow(oEdges)) {
  Edges <- oEdges[.(b0), nomatch = 0L]
  orig <- dest <- i.dest <- NULL
  print(Edges)
  if (!nrow(Edges)) {
    return(max_n)
  }
  if (b %in% Edges[[2]]) {
    return(1L)
  }
  setnames(Edges, 1:2, c("orig", "dest"))
  Edges2 <- Edges[Edges, on = .(dest = orig), nomatch = 0L]
  Edges2 <- Edges2[orig == b0, .(orig = dest, dest = i.dest)]
  Edges2 <- unique(Edges2)
  if (!nrow(Edges2)) {
    return(max_n)
  }
  setkey(Edges2, orig, dest)
  cat(b0, b, max_n)
  
  min(vapply(Edges2[["dest"]],
             function(bb) {
               len_b(bb, b, Edges2, max_n) + 1L
             }, FUN.VALUE = 0L)) 
}


path_from_edges <- function(a, b, Edges, current_path = NULL) {
  
  stopifnot(is.data.table(Edges), 
            identical(key(Edges)[1:2], names(Edges)[1:2]),
            is.integer(a), 
            is.integer(b),
            length(a) == 1,
            length(b) == 1)
  # print(current_path)
  if (!length(current_path)) {
    if (a %in% Edges[[1]]) {
      current_path <- a
    } else {
      return(NULL)
    }
  }
  if (a > b) {
    return(NULL)
  }
  
  
  if (!reaches_b(a, b, Edges)) {
    return(NULL)
  }
  
  newEdge <- Edges[.(a), nomatch = 0L]
  if (nrow(newEdge) == 0) {
    return(NULL)
  }
  dests <- newEdge[[2]]
  m <- match(b, dests, nomatch = 0L)
  if (m) {
    return(c(current_path, dests[m]))
  }
  
  for (j in seq_along(dests)) {
    # cat(j, " ")
    destj <- dests[j]
    if (!is.null(p <- path_from_edges(destj, b, Edges, current_path = c(current_path, destj)))) {
      return(p)
    }
  }
}
# for (i in 1:11) {
#   path_from_edges(99L, 200L, EdgeList2)
# }
# 
# 
# })

