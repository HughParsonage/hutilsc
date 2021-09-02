#' Layout of nodes from edge weights
#' 
#' @param edgelist A \code{data.table} giving the edges.
#' @param Ef,Et Integer vectors indicating the edges from and to.
#' 
#' @param weights,vcount,niter,max.delta,area,cool.exp,repulse.rad,init,groups,rotation,layout.control,constraints,round,digits As in the \code{qgraph} equivalent.
#' 
#' @export

qgraph.layout.fruchtermanreingold <- function(edgelist,
                                              Ef = .subset2(edgelist, key(edgelist)[1]),
                                              Et = .subset2(edgelist, key(edgelist)[2]),
                                              weights = rep(1, nrow(edgelist)),
                                              vcount = length(union(Ef, Et)),
                                              niter = 500L,
                                              max.delta = NULL,
                                              area = NULL,
                                              cool.exp = 1.5,
                                              repulse.rad = NULL,
                                              init = NULL,
                                              groups = NULL,
                                              rotation = NULL,
                                              layout.control = 0.5,
                                              constraints = NULL,
                                              round = TRUE, 
                                              digits = 5) {
  stopifnot(is.integer(Ef), is.integer(Et))
  # Provide default settings
  ecount <- nrow(edgelist)
  
  if (!is.null(vcount)) {
    n <- vcount 
  } else {
    # n <- max(length(unique(c(edgelist))),max(edgelist))
    n <- length(union(.subset2(edgelist, 1L),
                      .subset2(edgelist, 2L)))
    vcount <- n
  }
  if (is.null(weights)) {
    weights <- rep(1, ecount)
  }
  if (is.null(niter)) {
    niter <- 500
  }
  if (is.null(max.delta)) {
    max.delta <- as.double(n)
  }
  if (length(max.delta) == 1) {
    max.delta <- rep(max.delta, n)
  }
  if (is.null(area)) {
    area <- n^2
  }
  if (is.null(cool.exp)) { 
    cool.exp <- 1.5
  }
  if (is.null(repulse.rad)) {
    repulse.rad <- area*n
  }
  if (is.null(init)) {
    init <- matrix(0, nrow = n, ncol = 2)
    tl <- n + 1
    init[, 1] <- sin(seq(0, 2*pi, length = tl))[-tl] + rnorm(n, 0, 0.01)
    init[, 2] <- cos(seq(0, 2*pi, length = tl))[-tl] + rnorm(n, 0, 0.01)
  }
  
  
  x <- init[, 1]
  y <- init[, 2]
  
  # constraints:
  if (is.null(constraints)) {
    Cx <- Cy <- logical(vcount)
  } else {
    Cx <- !is.na(constraints[, 1])
    Cy <- !is.na(constraints[, 2])
    x[Cx] <- constraints[Cx, 1]
    y[Cy] <- constraints[Cy, 2]
  }
  
  
  
  # Round:
  if (round) {
    weights <- round(weights, digits)
    x <- round(x, digits)
    y <- round(y, digits)
  }
  
  
  
  layout <- 
    .Call("qgraph_layout_Cpp", 
          as.integer(niter),
          as.integer(n), 
          as.integer(ecount),
          as.double(max.delta),
          as.double(area), 
          as.double(cool.exp), 
          as.double(repulse.rad), 
          Ef,
          Et, 
          abs(weights), 
          as.double(x), 
          as.double(y), 
          as.logical(Cx), 
          as.logical(Cy),
          PACKAGE = packageName())
}
