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
#' @param Resistance If \code{NULL}, a \code{raw} vector indicating the 
#' resistance of the individual
#' 
#' @param Epi Epidemiological parameters.
#' 
#' 
#' @export simulate_racf
#' 
simulate_racf <- function(STP, 
                          Resistance = NULL,
                          keyz = c("id", "racf_abm"),
                          Epi = set_epipars(),
                          PatientZeroGroup = NULL,
                          PatientZero = sample(nrow(STP), size = 1),
                          n_days = 28L,
                          nThread = getOption("hutilsc.nThread", 1L)) {
  PREP <- prepare_racf(STP, keyz)
  u_id <- PREP[["u_id"]]
  stopifnot(is.integer(u_id))
  
  if (is.null(Resistance)) {
    if (hasName(STP, "Resistance")) {
      first_ids <- K1 != shift(K1, fill = -1L)
      Resistance <- as.raw(.subset2(STP, "Resistance")[first_ids])
    }
  }
  
  if (!is.raw(Resistance) || length(Resistance) != length(u_id)) {
    Resistance <- pcg_hash(length(u_id),
                           r = sample.int(1e9, size = 88),
                           raw_result = TRUE,
                           nThread = nThread)
  }
  
  .Call("Csimulate_racf", 
        PREP[[1]], PREP[[2]],
        PREP[[3]], PREP[[4]],
        PREP[[5]], PREP[[6]],
        Resistance,
        PatientZero - 1L, 
        n_days,
        list(), 
        nThread)
  
}


prepare_racf <- function(STP, keyz = c("id", "racf_abm")) {
  if (identical(key(STP), c("id", "racf_abn"))) {
    STP[, racf_abm := chmatch(racf_abn, unique(racf_abn))]
    setkey(STP, id, racf_abm)
  }
  
  if (STP[, first(id)] > 0L) {
    STP[, id := id - first(id)]
    setkey(STP, id, racf_abm)
  }
  
  u_id <- STP[, unique(id)]
  u_racf_abm <- STP[, unique(racf_abm)]
  
  stopifnot(identical(keyz, c("id", "racf_abm")))  # just hardcode for now
  stopifnot(is.data.table(STP),
            # id, racf
            length(key(STP)) >= 2L,
            keyz %in% names(STP),
            "n_employers_per_month" %in% names(STP))
  
  STP[, id := match(id, u_id) - 1L]
  STP[, racf_abm := match(racf_abm, u_racf_abm) - 1L]
  setkey(STP, id, racf_abm)
  
  id <- racf_abm <- NULL
  id_racf <- STP[, .(id, racf_abm, n_employers_per_month)]
  racf_id <- STP[, .(racf_abm, id)]
  setkey(id_racf, id, racf_abm)
  setkey(racf_id, racf_abm, id)
  
  K1 <- .subset2(id_racf, 1L)
  K2 <- .subset2(id_racf, 2L)
  J1 <- .subset2(racf_id, 1L)
  J2 <- .subset2(racf_id, 2L)
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
  
  iminmax_by_racf_id[, imin := imin ]
  iminmax_by_racf_id[, imax := imax ]
  
  IMIN <- .subset2(iminmax_by_racf_id, "imin")
  IMAX <- .subset2(iminmax_by_racf_id, "imax")
  
  list(K1, K2,
       J1, J2,
       IMIN, IMAX,
       u_id = u_id)
}



sample_prob <- function(n, prob) {
  n_false <- n * (1 - prob)
  n_true <- n - n_false
  sample(rep(c(FALSE, TRUE), 
             c(n_false, n_true)))
}




#' @rdname simulate_racf
#' @param sir \code{double{3}} A vector.
#' @export
set_epipars <- function(sir = c(1/5, 1/14),
                        incubation_distribution = c("dirac", "pois", "lnorm", "cauchy"), 
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





ShuffleRindex <- function(x) {
  .Call("CShuffleRindex", x, PACKAGE = packageName())
}

TestSimulate <- function(nn = 1e3) {
  STP <- fst::read_fst(Sys.getenv("R_ATO_RACF_STP_FST"), as.data = TRUE)
  STP_Dec <- STP[Month == "Dec"]
  STP_Dec[, i := .I]
  stopifnot(is.character(STP_Dec[["racf_abn"]]))
  # print(STP_Dec[racf_abn == "74082931575", .(i, id, racf_abn)])
  simulate_racf(STP_Dec, 
                PatientZero = STP_Dec[, which_last(racf_abn == "74082931575")], 
                nThread = 1L)
}

Simulate_all <- function(STP,
                         Resistance = NULL,
                         keyz = c("id", "racf_abm"),
                         Epi = set_epipars(),
                         n_days = 28L,
                         nThread = getOption("hutilsc.nThread", 1L)) {
  PREP <- prepare_racf(STP, keyz)
  u_id <- PREP[["u_id"]]
  
  if (is.null(Resistance)) {
    if (hasName(STP, "Resistance")) {
      first_ids <- K1 != shift(K1, fill = -1L)
      Resistance <- as.raw(.subset2(STP, "Resistance")[first_ids])
    }
  }
  
  if (!is.raw(Resistance) || length(Resistance) != length(u_id)) {
    Resistance <- pcg_hash(length(u_id),
                           r = sample.int(1e9, size = 88),
                           raw_result = TRUE, 
                           nThread = nThread)
  }
  iter <- 0:1e3
  Out <- 
    lapply(iter, function(i) {
      cat(formatC(i, width = 5L), "\r")
      Resistance <- pcg_hash(length(u_id),
                             r = sample.int(1e9, size = 88),
                             raw_result = TRUE, 
                             nThread = nThread)
      .Call("Csimulate_racf", 
            PREP[[1]], PREP[[2]],
            PREP[[3]], PREP[[4]],
            PREP[[5]], PREP[[6]],
            Resistance,
            i * 8L + 0:7, 
            n_days,
            list(), 
            nThread)
    })
  setDT(Out)
  setnames(Out, paste0("PZ", formatC(iter, width = 5L, flag = "0")))
  Out[]
}





