#' Color subgraphs
#' 
#' @param Edges A \code{data.table}, whose keys represent the edges of a graph.
#' As the graph is undirected, each edge must connect a node to a higher node.
#' 
#' @param Cliques A \code{data.table} containing at least two columns,
#' representing nodes in \code{Edges} and the clique of each such node.
#' 
#' @param nodes,clique Strings, the names of the column in \code{Cliques}
#' as specified.
#' 
#' @return A \code{data.table} of two columns, the first being the 
#' vertices of both columns of \code{DT} and the second being an 
#' integer vector, showing for each vertex the \code{"clique"}
#' to which it belongs.
#' 
#' @examples
#' library(data.table)
#' DT <- data.table(x = rpois(30, 15), y = rpois(30, 15))
#' DT <- DT[x != y]
#' DT[, x1 := pmin(x, y)][, x2 := pmax(x, y)]
#' DT[, x := NULL][, y := NULL]
#' setkey(DT, x1, x2)
#' color_clique(DT)
#' 
#' #                       #   #  ##  ##  ##  ## 
#' DT <- data.table(x = c(1L, 1L, 2L, 4L, 8L, 9L),
#'                  y = c(2L, 3L, 4L, 5L, 9L, 10L))
#' setkey(DT, x, y)
#' color_clique(DT)
#' 
#' 
#' @export

color_clique <- function(Edges) {
  DT <- Edges
  stopifnot(is.data.table(DT))
  kdt <- key(DT)
  if (length(kdt) < 2) {
    stop("key(Edges) has length < 2") # nocov
  }
  k1 <- kdt[1]
  k2 <- kdt[2]
  
  stopifnot(is.integer(K1 <- .subset2(DT, k1)),
            is.integer(K2 <- .subset2(DT, k2)))
  K1 <- .subset2(DT, k1)
  K2 <- .subset2(DT, k2)
  u <- union(K1, K2)  
  u <- u[order(u)]
  
  # Insist on sequential
  K1 <- match(K1, u)
  K2 <- match(K2, u)
  F1 <- match(u, K1, nomatch = 0L)
  
  C <- C3 <- NULL
  cat(".")
  saveRDS(list(u = u, C3 = C3, K1 = K1, K2 = K2), "~/tmp.rds")
  C <- .Call("do_clique1", u, K1, K2, F1, PACKAGE = packageName())
  saveRDS(list(u = u, C3 = C3, K1 = K1, K2 = K2), "~/tmp.rds")
  cat(".")
  fuse3_ans <- .Call("do_fuse3", u, C, K1, K2, PACKAGE = packageName())
  C3 <- fuse3_ans[C]
  n_iter <- 0L
  while (n_iter < length(u) && 
         !identical(fuse3_ans, seq_along(fuse3_ans))) {
    n_iter <- n_iter + 1L
    Sys.sleep(1)
    cat("\niter = ", n_iter);
    Sys.sleep(0.01)
    cat("packageName = ", packageName())
    cat("<\n")
    fuse3_ans <- .Call("do_fuse3", u, C3, K1, K2, PACKAGE = packageName())
    cat(".")
    C3 <- fuse3_ans[C3]
    saveRDS(list(u = u, C3 = C3, K1 = K1, K2 = K2), "~/tmp.rds")
    C3 <- enseq(C3)
    cat(".")
  }
  
  # data.table(Node = u, C = C, Clique3 = C3, key = "Node")
  data.table(Node = u, Clique = C3)
}

#' @rdname color_clique
#' @export 
validate_cliques <- function(Edges, Cliques, 
                             nodes = names(Cliques)[1], 
                             clique = names(Cliques)[2]) {
  stopifnot(is.data.table(Edges),
            haskey(Edges), 
            length(key(Edges)) >= 2L)
  
  stopifnot(is.integer(k1 <- .subset2(Edges, key(Edges)[1])),
            is.integer(k2 <- .subset2(Edges, key(Edges)[2])))
  
  stopifnot(hasName(Cliques, nodes), 
            hasName(Cliques, clique),
            is.integer(u <- .subset2(Cliques, nodes)),
            is.integer(c <- .subset2(Cliques, clique)))
  
  .Call("do_validate_clique", k1, k2, u, c, PACKAGE = packageName())
}



