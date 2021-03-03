
# Hardcoded for switch statements
expect_equal(hutilsc:::Ctest_input_types(1:5, 1:5, 1:5, 1:5), 1L)
expect_equal(hutilsc:::Ctest_input_types(1L, 1L, 1L, 1L), 171L)