#' The number of unique elements in a key
#' @param x Either a \code{data.table} or a sorted vector.
#' @param k An integer or string denoting for which key the number of unique 
#' elements is sought.
#' By default, the first key of \code{x} is chosen. If \code{x} is an atomic
#' vector, \code{k} is ignored (as \code{x} is assumed to be the elements of the
#' key itself).
#' @export

kuniqueN <- function(x, k = 1L, silent = FALSE) {
  if (is.data.table(x)) {
    if (length(k) != 1L) {
      if (isTRUE(silent)) {
        return(0L)
      }
      stop("`length(k) = `", length(k), ", but must be length-one.")
    }
    keyz <- key(x)
    if (is.null(keyz)) {
      if (isFALSE(silent)) {
        warning("`x` is a data.table but has no key so 0 is being returned.")
      }
      return(0L)
    }
   
    if (is.character(k)) {
      if (k %in% keyz) {
        K <- .subset2(x, k)
        return(kuniqueN(K, silent = silent))
      } else {
        if (isFALSE(silent)) {
          warning("`x` is a data.table and `k = `", k, 
                  "` but the keys of x are ", toString(keyz), ", ",
                  "so 0 is being returned.")
        }
        return(0L)
      }
    } 
    if (is.integer(k)) {
      if (is.na(k) || k <= 0L || k > length(keyz)) {
        if (isFALSE(silent)) {
          warning("`x` is a data.table and `k = `", k, 
                  "` but the number of keys is ", length(keyz), ", ",
                  "so 0 is being returned.")
        }
        return(0L)
      }
      K <- .subset2(x, keyz[k])
      return(kuniqueN(K, silent = silent))
    }
    stop("`k` was type ", typeof(k), " but must integer or character.")
  }
  ans <- .Call("CkuniqueN", x, PACKAGE = packageName())
  if (!is.null(ans)) {
    return(ans)
  }
  if (isFALSE(silent)) {
    message("Not using C level technique, using fall-back.")
  }
  uniqueN(x)
}
