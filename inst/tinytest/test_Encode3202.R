
Validate3202 <- hutilsc:::Validate3202
Decode3202 <- hutilsc:::Decode3202
Encode3202 <- hutilsc:::Encode3202
Lookup4 <- hutilsc:::Lookup4
names2int <- hutilsc:::names2int

x <- c("320200000002", "320201000000")
expect_true(Validate3202(x))
expect_equal(Decode3202(Encode3202(x)), x)
x <- c("320200000002", "320201000000", NA)
expect_true(Validate3202(x))
expect_equal(Decode3202(Encode3202(x)), x)
y <- c("320200000002", "420201000000")
expect_error(Validate3202(y))
expect_error(Validate3202("12345678901"))
expect_error(Validate3202("a"))


expect_equal(Lookup4(names2int("Hugh", "Parsonage")), "HuPa")
expect_equal(Lookup4(names2int("", "")), "")

tensc <- as.character(tens <- as.integer(10^(0:7)))
expect_equal(hutilsc:::pad0(tensc, 9L),
             formatC(tens, digits = 8, flag = "0", format = "d"))

