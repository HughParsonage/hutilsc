#' Color subgraphs
#' 
#' @param DT A \code{data.table}, whose keys represent the edges of a graph.
#' As the graph is undirected, each edge must connect a node to a higher node.
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

color_clique <- function(DT) {
  stopifnot(is.data.table(DT))
  kdt <- key(DT)
  if (length(kdt) < 2) {
    stop("key(Edges) has length < 2")
  }
  k1 <- kdt[1]
  k2 <- kdt[2]
  
  stopifnot(is.integer(K1 <- .subset2(DT, k1)),
            is.integer(K2 <- .subset2(DT, k2)))
  K1 <- .subset2(DT, k1)
  K2 <- .subset2(DT, k2)
  u <- union(K1, K2)  
  u <- u[order(u)]
  u_by_i <- data.table(u, i = seq_along(u))
  # Reverse the edges to try the reverse action
  DTR <- DT[, .SD, .SDcols = c(kdt)]
  setnames(DTR, rev(kdt))
  
  K1 <- c(.subset2(DT, k1), .subset2(DTR, k1))
  K2 <- c(.subset2(DT, k2), .subset2(DTR, k2))
  
  # Insist on sequential
  K1 <- match(K1, u)
  K2 <- match(K2, u)
  
  DT_bound <- data.table(K1, K2, key = "K1,K2")
  
  K1 <- .subset2(DT_bound, 1L)
  K2 <- .subset2(DT_bound, 2L)
  
  vCliques <- .Call("do_clique1", 
                    u, K1, K2, K1, K2)
  
  data.table(u, vCliques, key = "u")
}

validate_colors <- function(DT, color = "color") {
  stopifnot(is.data.table(DT), haskey(DT), 
            length(key(DT)) >= 2L)
  
  stopifnot(is.integer(k1 <- .subset2(DT, key(DT)[1])),
            is.integer(k2 <- .subset2(DT, key(DT)[2])))
  
  stopifnot(hasName(DT, color), 
            is.integer(c <- .subset2(DT, color)))
  
  .Call("do_validate_colors", k1, k2, c)
}



