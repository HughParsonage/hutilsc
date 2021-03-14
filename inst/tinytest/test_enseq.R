
enseq <- hutilsc:::enseq

z <- c(1L, 1L, 3L, 55L, 6L)
expect_equal(enseq(z, TRUE),
             enseq(z, FALSE))

