#' Ego graphs from networks
#' @description Also known as the neighbourhood, though peculiar to 
#' social networks
#'
#' @param v Integer vector of vertices 
#' @param order Single integer, the maximum discrete distance between any vertex
#' in \code{v}
#' @param Edges A \code{data.table} describing unweighted, undirected edges
#' of a graph.
#' @param u Optionally, the integer vector of nodes.
#' 
#' @return A \code{data.table} of two columns, the first is a column
#' of nodes, matching \code{u}, and the second is the order of the 
#' given node relative to \code{v}, or \code{NA} if the order is larger
#' than \code{v}.
#' 
#' @export

ego_net <- function(v, order = 1L, Edges, u = NULL) {
  stopifnot(is.data.table(Edges), haskey(Edges), length(key(Edges)) == 2L)
  checkmate::assert_int(v, na.ok = FALSE)
  checkmate::assert_int(order, na.ok = FALSE)
  
  kk <- key(Edges)
  k1 <- .subset2(Edges, kk[1])
  k2 <- .subset2(Edges, kk[2])
  if (is.null(u)) {
    u <- union(k1, k2)
    u <- u[order(u)]
  }
  
  # Ensure we have u = 1:10
  uu <- unique(u)
  k1u <- match(k1, uu)
  k2u <- match(k2, uu)
  
  NEdges <- data.table(nk1 = k2u, nk2 = k1u)
  setkeyv(NEdges, c("nk1", "nk2"))
  # Note double negative  (reverse direction)
  nk1 <- .subset2(NEdges, "nk1")
  nk2 <- .subset2(NEdges, "nk2")
  vu <- match(v, uu, nomatch = 0L)
  checkmate::check_int(vu, na.ok = FALSE)
  checkmate::check_int(order, lower = 0L, na.ok = FALSE)
  
  ans <- .Call("do_ego_net",
               vu,
               order, k1u, k2u, nk1, nk2, length(uu), PACKAGE = packageName())
  if (is.null(ans)) {
    return(NULL)
  }
  data.table(Node = uu, Order = ans)
}



basic_ego_net <- function(v, order = 4L, Edges = data.table()) {
  origEdges <- copy(Edges)
  stopifnot(length(key(Edges)) >= 2)
  setnames(origEdges, key(origEdges)[1:2], c("x", "y"))
  vv <- v
  if (order == 0) {
    return(v)
  }
  vEdges <- origEdges[(x %in% vv) | (y %in% vv)]
  vv <- union(vEdges$x, vEdges$y)
  origEdges[x == v, ans := 0L]
  origEdges[(x %in% vv) | (y %in% vv), ans := coalesce(ans, 1L)] 
  if (order >= 2) {
    for (i in 2:order) {
      origEdges[(x %in% vv) | (y %in% vv), ans := coalesce(ans, i)] 
      vEdges <- origEdges[(x %in% vv) | (y %in% vv)] 
      vv <- sort(unique(c(vv, union(vEdges$x, vEdges$y))))
    }
  }
  origEdges[]
}



