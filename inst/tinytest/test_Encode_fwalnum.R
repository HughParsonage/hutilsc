library(hutilsc)
expect_error(hutilsc:::Encode_fwalnum(c(NA_character_)))
suppressWarnings({
  expect_equal(as.integer(hutilsc:::Encode_fwalnum(c(NA_character_, "abc"))),
               c(NA_integer_, 0L))
})
