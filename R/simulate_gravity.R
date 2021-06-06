


simulate_gravity <- function(Edges, g = 9, s = 1) {
  stopifnot(length(key(Edges)) >= 2)
  k1 <- .subset2(Edges, key(Edges)[1])
  k2 <- .subset2(Edges, key(Edges)[2])
  k1 <- ensure_integer(k1)
  k2 <- ensure_integer(k2)
  u <- unique(c(k1, k2))
  u <- u[order(u)]
  K1 <- match(k1, u)
  K2 <- match(k2, u)
  # J1 <- match(j1, u)
  # J2 <- match(j2, u)
  uz <- match(u, u)
  # C_LayoutFruchtermanReingold1(SEXP UU, SEXP K1, SEXP K2, SEXP WW,
  #                              SEXP GG, SEXP SS)
  "%||%" <- function(a, b) if (is.null(a)) b else a
  WW <- .subset2(Edges, 3L) %||% seq_along(uz)
  ans <- .Call("C_LayoutFruchtermanReingold1",
               uz, K1, K2, WW, 
               g, s,
               PACKAGE = packageName())
  as.data.table(ans)
}

