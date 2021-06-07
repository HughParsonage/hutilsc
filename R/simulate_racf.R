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

simulate_racf <- function(STP, Epi = set_epipars(),
                          PatientZeroGroup = NULL,
                          PatientZero = sample(nrow(STP), size = 1),
                          n_days = 28L) {
  stopifnot(is.data.table(STP),
            # id, racf
            length(key(STP)) >= 2L)
  stopifnot(hasName(STP, "n_employers_per_month"))
  
  keyz <- key(STP)
  key1 <- keyz[1]
  key2 <- keyz[2]
  
  K1 <- .subset2(STP, key1)
  stopifnot(is.integer(K1))
  
  if (is.character(.K2 <- .subset2(STP, key2))) {
    racf_abns <- unique(.K2)
    K2 <- chmatch(.K2, racf_abns)
  } else {
    racf_abns <- NULL
    K2 <- .subset2(STP, key2)
  }
  stopifnot(is.integer(K2))
  
  
  # Idea. 
  # Start with patient zero
  # Model P(internal transmission)
  # Model P(external transmission)
  stopifnot(hasName(Epi, "EPI_OK"),
            isTRUE(Epi[["EPI_OK"]]))
 
  # TEMP
  stopifnot(identical(Epi[["incubation_distribution"]], "dirac"),
            is.double(Epi[["incubation_mean"]]))
  STP[, "incubation" := Epi[["incubation_mean"]] ]
  
  p_vaccinated <- .subset2(Epi, "v_workplaces")
  prob <- c(p_vaccinated, 1 - p_vaccinated)
  
  # isVaccinated may be TRUE or FALSE by modelling
  if (!hasName(STP, "isVaccinated")) {
    STP[, "isVaccinated" := NA]
  } else {
    stopifnot(is.logical(.subset2(STP, "isVaccinated")))
  }
  
  isVaccinated <- NULL
  STP[, isVaccinated := coalesce(isVaccinated,
                                 sample(c(TRUE, FALSE), size = .N, replace = TRUE, prob = c(prob)))]
  
  if (is.null(PatientZeroGroup)) {
    PatientZeroGroup <- sample(K2, size = 1L)
  }
  if (is.character(PatientZeroGroup)) {
    stopifnot(is.character(racf_abns))
    PatientZeroGroup <- sample(chmatch(.K2, racf_abns), 
                               size = 1)
  }
  K2s_infected <- PatientZeroGroup
  which_PatientZeroGroup <- which(K2 %in% PatientZeroGroup)
  
  STP[, isInfected := FALSE]
  set(STP, 
      i = which_PatientZeroGroup, 
      j = "isInfected", 
      value = sample(c(logical(length(which_PatientZeroGroup) - 1L), TRUE)))
  
  q_workplace_rate <- Epi[["q_workplace_rate"]]
  r_workplace_rate <- Epi[["r_workplace_rate"]]
  
  multi_employer_ids <- STP[n_employers_per_month > 1, unique(id)]
  
  nComm <- 
    STP[n_employers_per_month > 1L,
        .SD,
        .SDcols = c(key1, key2)]
  
  nComm[, racf_abnm := match(racf_abn, racf_abns)]
  nComm[, racf_abn := NULL] 
  setkey(nComm, racf_abnm)
  STP[, racf_abnm := chmatch(racf_abn, racf_abns)]
  
  nComm <-
    merge(nComm[, .(id, orig = racf_abnm)], 
          nComm[, .(id, dest = racf_abnm)], 
          by = "id",
          allow.cartesian = TRUE) %>%
    .[, .N, keyby = .(orig, dest)] 
    
  n_infected <- 1L
  
  if (TRUE) {
    DatesInfected <-
      .Call("Csimulate_racf", K1, K2, K2, 
            PatientZero, 28L, list(), 
            PACKAGE = "hutilsc")
    STP[, "DatesInfected" := DatesInfected]
    return(STP[])
  }
  
  cn <- function(r, n, N) {
    trues <- as.integer(r * n)
    falses <- N - trues
    rep(c(FALSE, TRUE), c(falses, trues))
  }
  
  # Simulate
  for (day in 1:n_days) {
    STP[, runif01 := runif(.N)]
    
    
    
    # Internal
    STP[, (paste0("isInfected", day)) := FALSE]
    STP[and3s(racf_abnm %in% K2s_infected),
        (paste0("isInfected", day)) := sample(cn(r_workplace_rate, sum(isInfected), .N)),
        by = c(key2)]
    
    nInfected_by_RACF <- 
      STP[, .(nInfected = sum(isInfected)), keyby = c(key2)]
    nInfected_by_RACF[, racf_abnm := chmatch(.BY[[1]], racf_abns), by = c(key2)]
    
    multi_employer_infected_ids <- intersect(multi_employer_ids, 
                                             STP[, id[isInfected]])
    K2s_infected <- multi_employer_infected_ids
  }
  STP
  
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
      .Call("Csimulate_racf", K1, K2, K2, 
            i, 28L, list(), 
            PACKAGE = "hutilsc")
    })
  cat("\n")
  # STP[, "DatesInfected" := DatesInfected]
  # return(STP[])
  DatesInfected
}


