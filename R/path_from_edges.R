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

#' @name n_paths
#' @title Number of paths between vertices
#' @param s \code{int} A vertex in Edges. The origin of the paths.
#' @param v \code{int} Optionally a vertex between \code{s} and \code{t}.
#' @param t \code{int} A vertex in Edges. The destination.
#' @param Edges A \code{data.table} designating the edgelist.
#' 
#' 
#' @export

n_paths_stv <- function(s = NULL, v = NULL, t = NULL, Edges, nPaths = NULL) {
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


betweeness <- function(a, Edges) {
  .Len3Paths <- len_three_paths(Edges)
  .Len4Paths <- len_four_paths(Edges)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  a <- ensure_integer(a)
  u <- union(k1, k2)
  u <- u[order(u)]
  ans <- 0
  cat(format(Sys.time(), format = "%M:%S"), " n = ", length(u), "\n", sep = "")
  for (s in seq_along(u)) {
    if (s %in% .Len3Paths$V1) {
      for (t in seq_along(u)) {
        if (s >= t) {
          next
        }
        cat(formatC(s, width = 4), formatC(t, width = 4), "\r")
        st.Len3Paths <- .Len3Paths[.(s, u, t), .N, nomatch = 0L]
        st.Len4Paths <- .Len4Paths[.(s, u, u, t), .N, nomatch = 0L]
        if (!(st.Len4Paths || st.Len3Paths)) {
          break
        }
        stv.Len3Paths <- .Len3Paths[.(s, a, t), .N, nomatch = 0L]
        stv.Len4Paths <- 
          .Len4Paths[.(s, u, a, t), .N, nomatch = 0L] + 
          .Len4Paths[.(s, a, u, t), .N, nomatch = 0L]
        n_with_v <- st.Len3Paths
        ans <-  ans + (stv.Len3Paths + stv.Len4Paths) / (st.Len3Paths + st.Len4Paths)
      }
    }
  }
  cat(format(Sys.time(), format = "%M:%S"), "\n", sep = "")
  ans
}

betweeness4 <- function(Edges) {
  .Len4Paths <- len_four_paths(Edges)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- union(k1, k2)
  u <- u[order(u)]
  V1 <- .Len4Paths[["V1"]]
  V2 <- .Len4Paths[["V2"]]
  V3 <- .Len4Paths[["V3"]]
  V4 <- .Len4Paths[["V4"]]
  .Call("CBetweenessLen4", k1, k2, u, V1, V2, V3, V4, PACKAGE = packageName())
}

n_paths_between_given_dist <- function(Edges, MoltenDistances = NULL, return_dt = FALSE) {
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
      o <- rep_len(0L, length(x))
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

double_edgelist <- function(Edges) {
  k1 = key(Edges)[1]
  k2 = key(Edges)[2]
  out <- data.table(orig = c(Edges[[k1]], Edges[[k2]]),
                    dest = c(Edges[[k2]], Edges[[k1]]))
  setnames(out, c(k1, k2))
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

n_paths_svt0 <- function(s, v, t, Edges, MoltenDistances = NULL, double_edges = TRUE) {
  
  if (is.null(MoltenDistances)) {
    MoltenDistances <- melt_distances(Edges)
  }
  if (isTRUE(double_edges)) {
    Edges <- unique(double_edgelist(Edges))
  }
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
  .Call("Cn_paths_svt0", s, v, t, K1, K2, uz, J1, J2, D, PACKAGE = packageName()) #%/% (1L + double_edges)
}

n_paths_igraph <- function(s, v = NULL, t, Edges) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph required.")
  }
  Graph <- igraph::graph_from_data_frame(Edges, directed = FALSE)
  Paths <- igraph::all_shortest_paths(from = s, to = t, graph = Graph)
  
  if (is.null(v)) {
    return(length(Paths$res))
  }
  o <- 0L
  if (!length(Res <- Paths$res)) {
    return(0L)
  }
  vc <- as.character(v)
  sum(vapply(Res, function(r) vc %in% names(r), FUN.VALUE = 0L))
  
  
}

t5 <- function() {
  DT <- fst::read_fst("~/nComm_by_RACF_Jan.fst", as.data.table = TRUE)
  DT <- n_paths_between_given_dist(DT, return_dt = TRUE)
  DT[]
}

