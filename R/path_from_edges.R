#' Path Between Nodes From List of Edges
#' @param orig,dest Scalar integers, the start and end nodes of the putative path.
#' @param Edges A list of two integer vectors, giving each edge of the graph.
#' 
#' @return If a path from \code{orig} to \code{dest} exists, the nodes
#' between 
#' 

path_from_edges <- function(orig, dest, Edges, 
                            color_colname = NULL,
                            return. = c("list", "union")) {
  orig <- checkmate::asInt(orig)
  dest <- checkmate::asInt(dest)
  
  stopifnot(is.data.table(Edges), length(key(Edges)) == 2)
  
  if (is.null(color_colname)) {
    color_colname <- paste0(names(Edges), collapse = "_")
    color_subgraphs(Edges, new_col = color_colname)
  }
  stopifnot(hasName(Edges, color_colname))
  
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  
  if (dest %notin% k2) {
    return(integer(0))
  }
  
  r1 <- which_first(k1 == orig)
  r2 <- which_last(k1 >= dest)
  out <- logical(nrow(Edges))
  for (i in seq.int(r1, r2)) {
    if (out[i]) {
      next
    }
    k1i <- Edges[i, k1]
    tos <- Edges[.(k1i)][[key(Edges)[2]]]
    out[tos] <- TRUE
  }
  
  
  
  
}

is_valid_path <- function(path, Edges) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  
  .Call("do_is_valid_path", path, k1, k2, PACKAGE = packageName())
  
}

which_reached_between <- function(a, b, Edges) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  
  .Call("do_reaches_between", a, b, k1, k2, PACKAGE = packageName())
}




