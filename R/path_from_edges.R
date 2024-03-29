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





