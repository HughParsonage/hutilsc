#' Distance between nodes given edges
#' 
#' @param x,y Two vectors of nodes.
#' @param dt_edges The edges of the graph, given as a keyed data table.
#' Two nodes \code{n1, n2} are connected if there exists a row in \code{dt_edges}
#' with both \code{n1} and \code{n2} in the first two columns.
#' 
#' @return A vector the same length as \code{x}, giving the integer
#' distance between each element of \code{x} and \code{y}.
#' 
#' @examples
#' 
#' library(data.table)
#' edges <- data.table(orig = 1:5, dest = 2:6)
#' setkey(edges, orig, dest)
#' distance_via_edges(1L, 2L, edges)
#' distance_via_edges(1L, 3L, edges)
#' distance_via_edges(1L, 5L, edges)
#' 
#' 
#' @export

distance_via_edges <- function(x, y, dt_edges) {
  stopifnot(haskey(dt_edges), 
            identical(key(dt_edges)[1:2], names(dt_edges)[1:2]))
  
  .Call("one_edge_dist", x, y, dt_edges[[1]], dt_edges[[2]], PACKAGE = packageName())
}
