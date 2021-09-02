#' Number of paths between vertices
#' @param s \code{int} A vertex in Edges. The origin of the paths.
#' @param v \code{int} Optionally a vertex between \code{s} and \code{t}. If \code{NULL}, 
#' \code{n_paths_svt} returns the number of shortest paths between \code{s} and \code{t}.
#' @param t \code{int} A vertex in Edges. The destination.
#' @param Edges A \code{data.table} designating the edgelist.
#' @param nPaths A \code{data.table} indicating the number of paths between
#' \emph{pairs} of each vertex in \code{Edges}.
#' 
#' 
#' @export

n_paths_svt <- function(s = NULL, v = NULL, t = NULL, Edges, nPaths = NULL) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  key_names <- key(Edges)[1:2]
  
  if (is.null(nPaths)) {
    EdgesF <-
      rbind(Edges, 
            # Want the reverse direction too, hence use.names = FALSE
            setcolorder(copy(Edges), rev(key_names)),
            use.names = FALSE)
    setkeyv(EdgesF, key_names)
    nPaths0 <- n_paths_between_given_dist(EdgesF, return_dt = TRUE)
    if (is.null(v)) {
      return(nPaths0[.(s, t), n_paths])
    }
    
    nPaths <- merge(nPaths0, nPaths0, 
                    by.x = key_names[2], 
                    by.y = key_names[1],
                    allow.cartesian = TRUE, 
                    suffixes = c(".x", ".y"))
    nPaths_names3 <- c(key_names, paste0(key_names[2], ".y"))
    setkeyv(nPaths, nPaths_names3)
    set_cols_first(nPaths, nPaths_names3)
  }
  stopifnot(is.data.table(nPaths), ncol(nPaths) >= 3)
  stopifnot(hasName(nPaths, paste0(key_names[2], ".y")))
  
  # Want the names to be the same for the merge
  nPaths_names3 <- head(names(nPaths), 3)
  stopifnot(head(names(Edges), 2) == head(names(nPaths), 2))
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- union(k1, k2)
  u <- u[order(u)]
  if (!length(u) || is.na(last(u))) {
    stop("length(u) == 0 or u = NA.")
  }
  if (!is.null(s)) {
    # checkmate::assert_int(s, lower = u[1], upper = last(u))
  } else {
    s <- u
  }
  if (!is.null(t)) {
    # checkmate::assert_int(t, lower = u[1], upper = last(u))
  } else {
    t <- u
  }
  nPaths[, n_paths_xy := n_paths.x * n_paths.y]
  
  if (!is.null(v)) {
    # checkmate::assert_int(v, lower = u[1], upper = last(u))
    return(nPaths[.(s, v, t), n_paths_xy])
  } else {
    nPaths_st <-
      nPaths[, .(n_paths_xy = sum(n_paths_xy)), keyby = c(nPaths_names3[1], nPaths_names3[3])] 
    return(nPaths_st[.(s, t), n_paths_xy])
  }
}
