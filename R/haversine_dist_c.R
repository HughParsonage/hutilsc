#' Haversine distance 
#' 
#' @param lat1,lon1,lat2,lon2 \describe{
#' \item{\code{double(N)}}{Coordinates between which the distances are required.}
#' } 
#' @param use_float Whether to allow a more imprecise square root function.
#' Set to \code{TRUE} for performance; set to \code{FALSE} for accuracy. (Though
#' note well that even setting \code{TRUE} will not be totally accurate
#' in measuring the real-world distance between two coordinates.)
#' 
#' @export

haversine_dist_c <- function(lat1, lon1, lat2, lon2, use_float = TRUE) {
  .Call("do_haversine_distance", 
        lat1, lon1, lat2, lon2, isTRUE(use_float))
}
