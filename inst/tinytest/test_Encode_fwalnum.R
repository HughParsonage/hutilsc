expect_error(Encode_fwalnum(c(NA_character_)))
suppressWarnings({
  expect_equal(Encode_fwalnum(c(NA_character_, "abc")), c(NA_integer_, 0L))
})