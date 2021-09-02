#' Number of paths between vertices
#' @param s \code{int} A vertex in Edges. The origin of the paths.
#' @param v \code{int} Optionally a vertex between \code{s} and \code{t}. If \code{NULL}, 
#' \code{n_paths_svt} returns the number of shortest paths between \code{s} and \code{t}.
#' @param t \code{int} A vertex in Edges. The destination.
#' @param Edges A \code{data.table} designating the edgelist.
#' @param weight_col Which column of \code{Edges} should be considered the edge 
#' weight? Either an \code{int} or a string.

#' @param MoltenDistances A \code{data.table} indicating the distance between 
#' each pair of vertices in \code{Edges}.
#' @param double_edges \code{TRUE | FALSE} Whether or not to double the 
#' \code{Edges} before analysing.
#' @param nThread Number of threads to use.
#' 
#' 
#' 
#' @export

n_paths_svt0 <- function(s, v = NULL, t, Edges,
                         weight_col = 3L,
                         MoltenDistances = NULL,
                         double_edges = TRUE,
                         nThread = getOption("hutilsc.nThread", 1L)) {
  stopifnot(is.data.table(Edges), haskey(Edges), 
            identical(key(Edges)[1:2], names(Edges)[1:2]))
  if (is.null(MoltenDistances)) {
    MoltenDistances <- melt_distances(Edges)
  }
  if (isTRUE(double_edges)) {
    Edges <- setkeyv(unique(double_edgelist(Edges)), key(Edges))
  }
  j1 <- .subset2(MoltenDistances, 1L)
  j2 <- .subset2(MoltenDistances, 2L)
  D <- .subset2(MoltenDistances, 3L)
  stopifnot(length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  W <- NULL
  if (is.numeric(weight_col) && isTRUE(weight_col <= length(Edges))) {
    W <- .subset2(Edges, weight_col)  
  }
  if (is.character(weight_col)) {
    W <- .subset2(Edges, weight_col)
  }
  
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- unique(c(k1, k2, j1, j2))
  u <- u[order(u)]
  if (length(u) > 255 && is.null(v)) {
    stop("The number of nodes, length(u) = ", length(u), " > 255, which is not permitted.")
  }
  
  cc <- identity
  cc(K1 <- match(k1, u))
  cc(K2 <- match(k2, u))
  cc(J1 <- match(j1, u))
  cc(J2 <- match(j2, u))
  uz <- match(u, u)
  
  
  
  if (is.null(v)) {
    .Call("Cn_paths_svt0", s, v, t, K1, K2, W, uz, J1, J2, D, nThread, PACKAGE = packageName())
  } else {
    .Call("Cn_paths_svt0",
          s, v, t, 
          K1, K2, W,
          uz,
          J1, J2, D, nThread, 
          PACKAGE = packageName())
  }
}

