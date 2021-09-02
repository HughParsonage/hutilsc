library(data.table)
library(hutilsc)
library(tinytest)

STP <- data.table(x = rep(1:10, 2L),
                  y = rep(101:110, each = 2L),
                  key = "x,y")

expect_equal(simulate_racf(STP,
                           Returner = 2L,
                           Resistance = raw(1),
                           PatientZero = 1L, 
                           Epi = list(r_workplace = 3L)), 
             2L)
expect_equal(simulate_racf(STP,
                           Returner = 2L,
                           Resistance = as.raw(0xff), 
                           PatientZero = 1L),
             1L)



if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_seed(1, {
    DT <- data.table(id = sample.int(10, size = 20, replace = TRUE),
                     racf_abm = sample.int(5, size = 20, replace = TRUE),
                     key = "id,racf_abm")
    DT[, n_employers_per_month := .N, by = .(id)]
  })
}
