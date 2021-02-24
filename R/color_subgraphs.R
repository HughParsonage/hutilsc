#' Color subgraphs
#' 
#' @param DT A \code{data.table}, whose keys represent the edges of a graph.
#' As the graph is undirected, each edge must connect a node to a higher node.
#' 
#' @param new_col \code{character(1)} The new column name for the coloring.
#' 
#' @param verbose Whether to display verbose info.
#' 
#' @return \code{DT} with an extra integer column, identifying each subgraph.
#' 
#' @examples
#' library(data.table)
#' DT <- data.table(x = rpois(30, 15), y = rpois(30, 15))
#' DT <- DT[x != y]
#' DT[, x1 := pmin(x, y)][, x2 := pmax(x, y)]
#' DT[, x := NULL][, y := NULL]
#' setkey(DT, x1, x2)
#' color_subgraphs(DT)
#' 
#' #                       #   #  ##  ##  ##  ## 
#' DT <- data.table(x = c(1L, 1L, 2L, 4L, 8L, 9L),
#'                  y = c(2L, 3L, 4L, 5L, 9L, 10L))
#' setkey(DT, x, y)
#' color_subgraphs(DT)
#' 
#' 
#' @export

color_subgraphs <- function(DT, new_col = "color", verbose = getOption("hutilsc.verbose", FALSE)) {
  stopifnot(is.data.table(DT), haskey(DT), 
            length(key(DT)) >= 2L)
  
  stopifnot(is.integer(k1 <- .subset2(DT, key(DT)[1])),
            is.integer(k2 <- .subset2(DT, key(DT)[2])))
  
  
  color <- .Call("do_color_graph", k1, k2, verbose, PACKAGE = packageName())
  set(DT, j = new_col, value = color)
  if (verbose) {
    print(DT)
  }
  
  # Now we need to account for endpoints
  DT[, "min_color" := min(color), by = c(key(DT)[2])]
  new_min_color <- pmin(color, .subset2(DT, "min_color"))

  ans <- fuse2(new_min_color, color)
  if (verbose) {
    cat("ans done\n")
  }
  DT[, (new_col) := NULL]
  set(DT, j = new_col, value = ans)
  
  DT[]
}

fuse2 <- function(x, y) {
  .Call("do_fuse2", x, y)
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

color_clique <- function(DT, new_color = "clique") {
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
  
  data.table(u, vCliques)
        
}

Rev <- function(x) .Call("test_rev", x)

eloop <- function(x, a, b) {
  .Call("test_loop", x, a, b, PACKAGE = packageName())
  
}

