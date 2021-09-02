#' Number of paths when distance is known (or knowable)
#' @param Edges A \code{data.table} giving the edges.
#' @param MoltenDistances A \code{data.table} indicating the distance between
#' each node in \code{Edges}. 
#' @param return_dt \code{TRUE | FALSE} Should a \code{data.table} be returned?
#' If \code{TRUE}, it will be appended to \code{MoltenDistances}.
#' If \code{FALSE}, a vector indicating the number of (shortest) paths between
#' each node. 
#' 
#' @export
n_paths_between_given_dist <- function(Edges, MoltenDistances = NULL, return_dt = TRUE) {
  if (is.null(MoltenDistances)) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("igraph not available, yet Distances = NULL.")
    }
    Graph <- igraph::graph_from_data_frame(Edges, directed = FALSE)
    Distances <- igraph::distances(Graph)
    MoltenDistances <-
      melt(as.data.table(Distances, keep.rownames = "orig"),
           id.vars = "orig",
           variable.name = "dest",
           variable.factor = FALSE,
           value.name = "dist")
    
    as_int_inf <- function(x) {
      if (is.integer(x)) {
        return(x)
      }
      if (!is.double(x)) {
        return(as.integer(x))
      }
      # handle Inf at c level (Infinite distance imply no paths)
      o <- integer(length(x))
      o[is.finite(x)] <- as.integer(x[is.finite(x)])
      o
    }
    
    MoltenDistances <- MoltenDistances[, lapply(.SD, as_int_inf)]
    setkeyv(MoltenDistances, c("orig", "dest"))
    hutils::set_cols_first(MoltenDistances, c("orig", "dest"))
  }
  stopifnot(is.data.frame(MoltenDistances),
            length(MoltenDistances) == 3L)
  j1 <- .subset2(MoltenDistances, 1L)
  j2 <- .subset2(MoltenDistances, 2L)
  D <- .subset2(MoltenDistances, 3L)
  stopifnot(length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- unique(c(k1, k2, j1, j2))
  u <- u[order(u)]
  K1 <- match(k1, u)
  K2 <- match(k2, u)
  J1 <- match(j1, u)
  J2 <- match(j2, u)
  uz <- match(u, u)
  ans <- .Call("C_nPathsBetween_GivenDist", K1, K2, uz, J1, J2, D, PACKAGE = packageName())
  if (isTRUE(return_dt)) {
    MoltenDistances[, "n_paths" := ans]
    return(MoltenDistances[])
  }
  ans
}

