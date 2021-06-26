withr::with_dir("~/hutilsc", {
  de_in() # load library and
  libraries()
})
STP <- fst::read_fst(Sys.getenv("R_ATO_RACF_STP_FST"), as.data = TRUE)
STP_Dec <- STP[Month == "Dec"]
STP_Dec[, i := .I]
stopifnot(is.character(all_abns <- unique(STP_Dec[["racf_abn"]])))
STP <- STP_Dec
options(hutilsc.nThread = 6L)
stopifnot(Sys.Date() == "2021-06-24")
stopifnot("run.R" %in% dir())
for (abni in seq_along(all_abns)) {
  abn <- all_abns[abni]
  STP[, Resistance := pcg_hash(.N, raw_result = TRUE)]
  STP[racf_abn == all_abns[abni], Resistance := as.raw(0xff)]
  Simulate_all(STP, out.tsv = paste0(abn, "-vacc-", format(Sys.time(), "%m-%d-%H%M%S"), ".tsv"))
}



