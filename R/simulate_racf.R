#' Simulate transmission through bipartite graph
#' 
#' @param STP A data.table with two keys. The first key is the 
#' identifier of the person; the second key is the identifer of the group
#' of the person.
#' 
#' If STP has a column \code{"isVaccinated"}, it must be a logical column.
#' If present, it may be used to indicate which rows are vaccinated. If 
#' a row is \code{NA}, it will be replaced with \code{TRUE} or \code{FALSE}
#' based on the epidemiological parameter \code{Epi$v_workplaces}.
#' 
#' @param Epi Epidemiological parameters.
#' 
#' @export
#' 

simulate_racf <- function(STP, 
                          keyz = c("id", "racf_abm"),
                          Epi = set_epipars(),
                          PatientZeroGroup = NULL,
                          PatientZero = sample(nrow(STP), size = 1),
                          n_days = 28L,
                          nThread = getOption("hutilsc.nThread", 1L)) {
  stopifnot(missing(keyz))  # just hardcode for now
  stopifnot(is.data.table(STP),
            # id, racf
            length(key(STP)) >= 2L,
            keyz %in% names(STP),
            "n_employers_per_month" %in% names(STP))
  
  id <- racf_abm <- NULL
  id_racf <- STP[, .(id, racf_abm, n_employers_per_month)]
  racf_id <- STP[, .(racf_abm, id)]
  setkey(id_racf, id, racf_abm)
  setkey(racf_id, racf_abm, id)
  
  K1 <- .subset2(id_racf, 1L) - 1L
  K2 <- .subset2(id_racf, 2L) - 1L
  J1 <- .subset2(racf_id, 1L) - 1L
  J2 <- .subset2(racf_id, 2L) - 1L
  stopifnot(is.integer(K1),
            is.integer(K2),
            is.integer(J1),
            is.integer(J2))
  racf_id[, n_employees := .N, by = .(racf_abm)]
  racf_id[, i := .I]
  
  iminmax_by_racf_id <- 
    racf_id[, .(imin = min(i),
                imax = max(i)), 
            keyby = .(racf_abm)]
  
  iminmax_by_racf_id[, imin := imin - 1L]
  iminmax_by_racf_id[, imax := imax - 1L]
  
  IMIN <- .subset2(iminmax_by_racf_id, "imin")
  IMAX <- .subset2(iminmax_by_racf_id, "imax")
  
  .Call("Csimulate_racf", 
        K1, K2,
        J1, J2,
        IMIN, IMAX,
        PatientZero, 
        28L,
        list(), 
        nThread)
  
  
}

sample_prob <- function(n, prob) {
  n_false <- n * (1 - prob)
  n_true <- n - n_false
  sample(rep(c(FALSE, TRUE), 
             c(n_false, n_true)))
}




#' @rdname simulate_racf
#' @export
set_epipars <- function(incubation_distribution = c("dirac", "pois", "lnorm", "cauchy"), 
                        incubation_mean = 8,
                        incubation_sigma = 0.44,
                        v_workplaces = 0.1,
                        q_workplace_rate = 1/14,
                        r_workplace_rate = 2) {
  incubation_distribution <- match.arg(incubation_distribution)
  checkmate::assert_number(incubation_mean)
  checkmate::assert_number(incubation_sigma)
  checkmate::assert_number(v_workplaces, lower = 0, upper = 1)
  checkmate::assert_number(q_workplace_rate, lower = 0, upper = 1)
  
  data.table(EPI_OK = TRUE,
             incubation_distribution,
             incubation_mean,
             incubation_sigma,
             v_workplaces,
             q_workplace_rate,
             r_workplace_rate)
}

pcg_hash <- function(n, r = NULL, nThread = getOption("hutilsc.nThread", 1L)) {
  if (is.null(r)) {
    r <- .Random.seed
  }
  .Call("Cpcg_hash", as.double(n), r, nThread, PACKAGE = packageName()) 
}

ShuffleRindex <- function(x) {
  .Call("CShuffleRindex", x, PACKAGE = packageName())
}

TestSimulate <- function(nn = 1e3) {
  STP <- fst::read_fst("~/dhhs/ATO-RACF/data/STP.fst", as.data = TRUE)
  K1 <- STP$id
  K2 <- chmatch(STP$racf_abn, unique(STP$racf_abn))
  formatted <- format(seq_len(nn))
  DatesInfected <-
    lapply(seq_len(nn), function(i) {
      cat(formatted[i], "\r")
      # .Call("Csimulate_racf", K1, K2, K2, 
      #       i, 28L, list(), 
      #       PACKAGE = "hutilsc")
    })
  cat("\n")
  # STP[, "DatesInfected" := DatesInfected]
  # return(STP[])
  DatesInfected
}

CountRaws <- function(x, nThread = getOption("hutilsc.nThread", 1L)) {
  .Call("CCountRaws", x, nThread, PACKAGE = packageName())
}

