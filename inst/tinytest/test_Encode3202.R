
x <- c("320200000002", "320201000000")
expect_true(Validate3202(x))
expect_equal(Decode3202(Encode3202(x)), x)
y <- c("320200000002", "420201000000")
expect_error(Validate3202(y))
expect_error(Validate3202("1234567890"))


expect_equal(Lookup4(names2int("Hugh", "Parsonage")), "HuPa")

tensc <- as.character(tens <- as.integer(10^(0:7)))
expect_equal(pad0(tensc, 9),
             formatC(tens, digits = 9, flag = "0", format = "d"))

