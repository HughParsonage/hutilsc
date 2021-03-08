library(hutilsc)

expect_equal(haversine_dist_c(0, 0, 1, 1, use_float = FALSE), 157.2494, tolerance = 0.001)
expect_equal(haversine_dist_c(0, 0, 1, 1, use_float = TRUE),  157.2494, tolerance = 0.001)

