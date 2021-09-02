#' Path Between Nodes From List of Edges
#' @param path Integer vector, a candidate path.
#' @param a,b Scalar integers. The origin and destination of a path.
#' @param Edges A list of two integer vectors, giving each edge of the graph.
#' 
#' @param len For \code{common_contacts}, the length of the path to consider.
#' 
#' @return If a path from \code{orig} to \code{dest} exists, \code{is_valid_path}
#' returns \code{TRUE}.
#' 
#' \code{common_contacts} returns the nodes that are between the nodes.
#' 
#' @export is_valid_path

is_valid_path <- function(path, Edges) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  path <- ensure_integer(path)
  
  .Call("Cis_valid_path", path, k1, k2, PACKAGE = packageName())
  
}

#' @rdname is_valid_path
#' @export
common_contacts <- function(a, b, Edges, len = 3L) {
  if (!identical(len, 3L)) {
    .NotYetImplemented()
  }
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  u <- union(k1, k2)
  u <- u[order(u)]
  
  
  
  if (a > b || (a %notin% u) || (b %notin% u)) {
    return(integer(0))
  }
  .Call("Ccommon_contacts", a, b, k1, k2, u, len, PACKAGE = packageName())
}

len_three_paths <- function(Edges, set_key = TRUE) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- union(k1, k2)
  u <- u[order(u)]
  k1 <- match(k1, u)
  k2 <- match(k2, u)
  out <- .Call("len3_paths", k1, k2, u)
  setDT(out)
  if (!nrow(out)) {
    return(data.table())
  }
  setnames(out, paste0("V", 1:3))
  out <- out[, lapply(.SD, function(x) u[x])]
  if (isTRUE(set_key)) {
    keys <- names(out)[1:2]
    setkeyv(out, keys)
  }
  out[]
}

len_four_paths <- function(Edges, set_key = TRUE) {
  stopifnot(is.data.table(Edges), length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- union(k1, k2)
  u <- u[order(u)]
  k1 <- match(k1, u)
  k2 <- match(k2, u)
  Len3Paths <- .Call("len3_paths", k1, k2, u)
  out <- .Call("len4_paths", Len3Paths, k1, k2, u)
  setDT(out)
  # Reset names to original
  out <- out[, lapply(.SD, function(x) u[x])]
  setnames(out, paste0("V", 1:4))
  if (isTRUE(set_key)) {
    keys <- names(out)[1:3]
    setkeyv(out, keys)
  }
  out[]
}




dist_bw_edges <- function(Edges) {
  stopifnot(length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- unique(c(k1, k2))
  u <- u[order(u)]
  K1 <- match(k1, u)
  K2 <- match(k2, u)
  ans_ <- .Call("Cdist_bw_edges", K1, K2, u, PACKAGE = packageName())
  CJ(orig = u, dest = u)[, dist := ans_][]
}

dist_bw_edge_igraph <- function(Edges) {
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
  hutils::set_cols_first(MoltenDistances, c("orig", "dest"))[]
}




double_edgelist <- function(Edges) {
  k1 = key(Edges)[1]
  k2 = key(Edges)[2]
  out <- data.table(orig = c(Edges[[k1]], Edges[[k2]]),
                    dest = c(Edges[[k2]], Edges[[k1]]))
  if (length(Edges) >= 3L) {
    nom3 <- names(Edges)[3]
    out[, (nom3) := rep(.subset2(Edges, 3L), 2L)]
  }
  setnames(out, c("orig", "dest"), c(k1, k2))
  setkeyv(out, c(k1, k2))[]
}
is_symmetric <- function(X) {
  is.matrix(Distances) && NROW(X) == NCOL(X) && identical(t(Distances), Distances)
}

melt_distances <- function(Edges, Distances = NULL) {
  if (is.null(Distances)) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("igraph not available, yet Distances = NULL.")
    }
    Graph <- igraph::graph_from_data_frame(Edges, directed = FALSE)
    Distances <- igraph::distances(Graph)
  }
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



n_paths_igraph <- function(s, v = NULL, t, Edges) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph required.")
  }
  Graph <- igraph::graph_from_data_frame(Edges, directed = FALSE)
  Paths <- igraph::all_shortest_paths(from = as.character(s), 
                                      to = as.character(t),
                                      graph = Graph)
  
  if (is.null(v)) {
    return(length(Paths$res))
  }
  o <- 0L
  if (!length(Res <- Paths$res)) {
    return(0L)
  }
  if (length(Res) == 1) {
    return(as.integer(as.character(v) %in% names(Paths$res)))
  }
  DT_ <- as.data.table(t(sapply(Res, as.matrix)))
  DTU <- unique(DT_)
  o <- 0L
  for (j in seq.int(2L, ncol(DTU) - 1L, by = 1L)) {
    vj <- .subset2(DTU, j)
    o <- o + sum(vj == v)
  }
  return(o)
}

t5 <- function() {
  DT <- fst::read_fst("~/nComm_by_RACF_Jan.fst", as.data.table = TRUE)
  DT <- n_paths_between_given_dist(DT, return_dt = TRUE)
  DT[]
}




sqrt2 <- function(x) {
  .Call("Csqrt2", as.double(x), PACKAGE = "hutilsc")
}

showsqrt_fast_ <- function(x) {
  .Call("showsqrt_fast", x, PACKAGE = packageName())
}

myDists <- function() {
  hh_ss()
  suppressMessages(libraries())
  Edges <- fst::read_fst("~/nComm_by_RACF_Dec.fst", as.data = TRUE)
  u <- Edges[, union(orig, dest)]
  Edges[, orig := match(orig, u)]
  Edges[, dest := match(dest, u)]
  Edges[, c("orig", "dest") := ensure_leq(orig, dest)]
  setkey(Edges, orig, dest)
  Edges <- double_edgelist(Edges)
  # print(dist_bw_edges(Edges))
  dist_bw_edges(Edges)[, .N, keyby = .(dist)]
}

nFirstOrder <- function(id1, id2, nid2, nThread = 1L) {
  .Call("C_nFirstOrder", id1, id2, nid2, nThread, PACKAGE = "hutilsc")
}


Dist2 <- function(Edges) {
  stopifnot(names(Edges)[1] == "orig",
            names(Edges)[2] == "dest",
            is.integer(Edges$orig),
            is.integer(Edges$dest))
  Edges2 <- copy(Edges)
  
  Edges2[, c("orig", "dest") := ensure_leq(orig, dest)]
  setkey(Edges2, orig, dest)
  u <- Edges2[, union(orig, dest)]
  u <- sort(u)
  orig <- match(.subset2(Edges2, "orig"), u)
  dest <- match(.subset2(Edges2, "dest"), u)
  
  ans_ <- .Call("CDist2", orig, dest, u, PACKAGE = "hutilsc")
  CJ1 <- 
    CJ(orig = u[seq_along(u)], 
       dest = u[seq_along(u)])
  CJ1[, ans := ans_]
  CJ1[]
}

ig2df <- igraph::graph_from_data_frame
