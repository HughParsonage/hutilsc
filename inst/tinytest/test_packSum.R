
x <- rep_len(c(TRUE, FALSE, TRUE), 32)
expect_equal(hutilsc:::sum_pack(packBits(x, "raw")), sum(x))
expect_equal(hutilsc:::sum_pack(packBits(x, "integer")), sum(x))

