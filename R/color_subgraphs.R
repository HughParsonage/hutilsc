#' Color subgraphs
#' 
#' @param DT A \code{data.table}, whose keys represent the edges of a graph.
#' As the graph is undirected, each edge must connect a node to a higher node.
#' 
#' @param new_col \code{character(1)} The new column name for the coloring.
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

color_subgraphs <- function(DT, new_col = "color") {
  stopifnot(is.data.table(DT), haskey(DT), 
            length(key(DT)) >= 2L)
  
  stopifnot(is.integer(k1 <- .subset2(DT, key(DT)[1])),
            is.integer(k2 <- .subset2(DT, key(DT)[2])))
  
  color <- .Call("do_color_graph", k1, k2, PACKAGE = packageName())
  set(DT, j = new_col, value = color)
  DT[]
}

