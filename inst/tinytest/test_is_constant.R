library(hutilsc)
xl <- logical(5)
xi <- integer(5)
xd <- double(5)
xc <- character(5)
expect_true(is_constant(xl))
expect_true(is_constant(xi))
expect_true(is_constant(xd))
expect_true(is_constant(xc))

yl <- c(TRUE, xl)
yi <- c(1L, xi)
yd <- c(1, xd)
yc <- c(" ", xc)
expect_false(is_constant(yl))
expect_false(is_constant(yi))
expect_false(is_constant(yd))
expect_false(is_constant(yc))






