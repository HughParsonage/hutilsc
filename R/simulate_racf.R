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
#' @param Resistance A \code{raw} vector indicating the 
#' resistance of each individual.
#' If \code{NULL}, the default, the resistance is uniformly 
#' distributed between \code{00} (255/256 chance of infection) and
#'  \code{ff} (0/256 chance of infection). 
#'  
#' 
#' @param PatientZero \code{integer(nPatient)} An integer vector, the 
#' starting index cases. By default a random sample of the number of 
#' individuals is used.
#' 
#' @param PatientZeroGroup Not yet implemented.
#'   
#' 
#' @param Epi Epidemiological parameters.
#' 
#' @param n_days \code{integer(1)} Number of days to simulate.
#' 

#' 
#' @param Returner The return type. An integer.
#' \describe{
#' \item{\code{0}}{A raw vector of length }
#' }
#' @param nThread Number of threads to use.
#' 
#' @param verbose Print simulation logs?
#' 
#' @export
simulate_racf <- function(STP, 
                          Resistance = NULL,
                          Epi = set_epipars(),
                          PatientZeroGroup = NULL,
                          PatientZero = NULL,
                          n_days = 28L,
                          Returner = 0L,
                          nThread = getOption("hutilsc.nThread", 1L),
                          verbose = FALSE) {
  force(PatientZero)
  stopifnot(is.integer(Returner), 
            length(Returner) == 1L)
  Returner <- coalesce(Returner, 0L)
  
  if (missing(STP)) {
    STP <- fst::read_fst(Sys.getenv("R_ATO_RACF_STP_FST"), as.data.table = TRUE)
    STP_Dec <- STP[Month == "Dec"]
    STP_Dec[, i := .I]
    stopifnot(is.character(STP_Dec[["racf_abn"]]))
    STP <- STP_Dec
  } else {
    STP <- copy(STP)
  }
  if (is.null(PatientZero)) {
    PatientZero <- sample.int(kuniqueN(STP), size = 1)
  }
  PREP <- prepare_racf(STP)
  u_id <- PREP[["u_id"]]
  stopifnot(is.integer(u_id))
  
  if (is.null(Resistance)) {
    # if (hasName(STP, "Resistance")) {
    #   K1 <- .subset2(STP, key(STP)[1])
    #   first_ids <- K1 != shift(K1, fill = -1L)
    #   Resistance <- as.raw(.subset2(STP, "Resistance")[first_ids])
    # }
  }
  
  if (length(Resistance) == 1L) {
    Resistance <- rep.int(as.raw(Resistance), length(u_id))
  }
  
  if (!is.raw(Resistance) || length(Resistance) != length(u_id)) {
    Resistance <- pcg_hash(length(u_id),
                           r = sample.int(1e9, size = 88),
                           raw_result = TRUE,
                           nThread = nThread)
  }
  
  
  out <- 
    .Call("Csimulate_racf", 
          PREP[[1]], PREP[[2]],
          PREP[[3]], PREP[[4]],
          PREP[[5]], PREP[[6]],
          .subset2(STP, "R1"),
          Resistance,
          PatientZero - 1L, 
          n_days,
          Epi, 
          Returner,
          nThread,
          verbose)
  switch(as.character(Returner),
         "0" = out,
         "1" = out,
         "33" = out,
         out)
  
}


prepare_racf <- function(STP) {
  Resistance <- .subset2(STP, "Resistance")
  if (is.raw(Resistance)) {
    # Bad interplay with setkey
    
    STP[, Resistance := as.integer(Resistance)]
  }
  orig_keyz <- copy(key(STP))
  if (length(orig_keyz) < 2) {
    stop("length(key(STP)) = ", length(orig_keyz), ", but 2 key columns are required.")
  }
  orig_key1 <- orig_keyz[1]
  orig_key2 <- orig_keyz[2]

  if (!hasName(STP, "n_employers_per_month")) {
    STP[, n_employers_per_month := .N, by = c(orig_key1)]
  }
  setkeyv(STP, orig_keyz)
  ok1 <- .subset2(STP, orig_key1)
  ok2 <- .subset2(STP, orig_key2)
  
  u1_id <- unique_sorted(ok1)
  u2_id <- unique(ok2)
  ok1 <- fmatch(ok1, u1_id) - 1L
  ok2 <- fmatch(ok2, u2_id) - 1L
  
  DT1 <- data.table(ok1, ok2, key = c("ok1", "ok2"))
  DT2 <- data.table(ok2, ok1, key = c("ok2", "ok1"))
  K1 <- .subset2(DT1, 1L)
  K2 <- .subset2(DT1, 2L)
  J1 <- .subset2(DT2, 1L)
  J2 <- .subset2(DT2, 2L)
  stopifnot(is.integer(K1),
            is.integer(K2),
            is.integer(J1),
            is.integer(J2))
  DT2[, n_employees := .N, by = c("ok2")]
  DT2[, i := 0:(.N - 1L)]
  
  iminmax_by_racf_id <- 
    DT2[, .(imin = min(i),
            imax = max(i)), 
        keyby = c("ok2")]
  
  IMIN <- .subset2(iminmax_by_racf_id, "imin")
  IMAX <- .subset2(iminmax_by_racf_id, "imax")
  
  if (hasName(STP, "Resistance")) {
    STP[, Resistance := as.raw(Resistance)] # by reference
  }
  
  list(K1 = K1, 
       K2 = K2,
       J1 = J1,
       J2 = J2,
       IMIN = IMIN, 
       IMAX = IMAX,
       u_id = u1_id,
       multi_employer = .subset2(DT1, "n_employers_per_month") > 1L)
}



sample_prob <- function(n, prob) {
  n_false <- n * (1 - prob)
  n_true <- n - n_false
  sample(rep(c(FALSE, TRUE), 
             c(n_false, n_true)))
}

repe <- function(x, y) {
  .Call("Crepe", x, y, 6L, PACKAGE =)
}


#' @rdname simulate_racf
#' @param sir,incubation_distribution,incubation_mean,incubation_sigma,v_workplaces,q_workplace_rate 
#' Not yet used.
#' 
#' @param r_workplace The rate of reproduction per workplace.
#' 
#' @export
set_epipars <- function(sir = c(1/5, 1/14),
                        incubation_distribution = c("dirac", "pois", "lnorm", "cauchy"), 
                        incubation_mean = 8,
                        incubation_sigma = 0.44,
                        v_workplaces = 0.1,
                        q_workplace_rate = 1/14,
                        r_workplace = 2) {
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
             r_workplace)
}





ShuffleRindex <- function(x) {
  .Call("CShuffleRindex", x, PACKAGE = packageName())
}

TestSimulate <- function(nn = 1e3) {
  simulate_racf(PatientZero = 1:nn, Returner = 2L,
                nThread = 1L)
}

Simulate_all <- function(STP,
                         Resistance = NULL,
                         Returner = 1L,
                         keyz = c("id", "racf_abm"),
                         Epi = set_epipars(),
                         iter_head = 9536L,
                         n_days = 28L,
                         out.tsv = NULL,
                         nThread = getOption("hutilsc.nThread", 1L)) {
  if (missing(STP)) {
    STP <- fst::read_fst(Sys.getenv("R_ATO_RACF_STP_FST"), as.data = TRUE)
    STP_Dec <- STP[Month == "Dec"]
    STP_Dec[, i := .I]
    stopifnot(is.character(STP_Dec[["racf_abn"]]))
    STP <- STP_Dec
  }
  PREP <- prepare_racf(STP)
  u_id <- PREP[["u_id"]]
  stopifnot(hasName(PREP, "multi_employer"))
  
  if (is.null(Resistance)) {
    if (hasName(STP, "Resistance")) {
      K1 <- PREP[["K1"]]
      first_ids <- K1 != shift(K1, fill = -1L)
      Resistance <- as.raw(.subset2(STP, "Resistance")[first_ids])
    }
  }
  
  if (!is.raw(Resistance) || length(Resistance) != length(u_id)) {
    if (length(Resistance) == 1) {
      Resistance <- rep_len(Resistance, length(u_id))
    } else {
      Resistance <- pcg_hash(length(u_id),
                             r = sample.int(1e9, size = 88),
                             raw_result = TRUE, 
                             nThread = nThread)
      Resistance <- rep_len(Resistance[Resistance <= 128], length(Resistance))
    }
  }
  # Only simulate multi-employer patients
  iter <- which(PREP[["multi_employer"]]) - 1L
  iter <- head(iter, iter_head)
  cat(hh_ss(), "\n")
  Out <- 
      .Call("Csimulate_racf", 
            PREP[[1]], PREP[[2]],
            PREP[[3]], PREP[[4]],
            PREP[[5]], PREP[[6]],
            rpois(length(PREP[[1]]), 1/8),
            Resistance,
            iter, 
            n_days,
            list(), 
            Returner,
            nThread,
            FALSE)
  Sys_time <- Sys.time()
  ans <- 
    switch(Returner + 1L,
           
           data.table(InfectionDate = Out,
                      Patient = rep_len(u_id, length(Out)),
                      Patient0 = rep(iter, each = length(u_id) - 1)),
           data.table(Out, Patient0 = u_id[iter]))
  if (is.null(out.tsv)) {
    out.tsv <- (paste0("data-raw/", format(Sys_time, "%d%H%M%S"), ".tsv"))
  }
  hutils::provide.file(out.tsv)
  fwrite(ans, 
         file = out.tsv,
         sep = "\t")
  ans
}

test_qru <- function(p, q, m = 0L, z = 5L, n = 1e5L) {
  ai <- as.integer
  stopifnot(length(p) == 1, p <= q)
  .Call("Ctest_qru", ai(p), ai(q), ai(m), ai(z), ai(n), PACKAGE = packageName())
}




