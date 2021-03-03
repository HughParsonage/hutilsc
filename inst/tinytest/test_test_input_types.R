
# Hardcoded for switch statements
expect_equal(hutilsc:::Ctest_input_types(1:5, 1:5, 1:5, 1:5), 1L)
expect_equal(hutilsc:::Ctest_input_types(1:5, 1:5, 1:5, 1:5), 1L)
expect_equal(hutilsc:::Ctest_input_types(1L, 1L, 1L, 1L), 171L)
expect_equal(hutilsc:::Ctest_input_types(1:5, 1L, 1:5, 1L), 11L)  # IiIi
expect_equal(hutilsc:::Ctest_input_types(1:5, pi, 1:5, pi), 16L)  # IrIr

expect_equal(hutilsc:::Ctest_input_types(1:5 + 0, pi, 1L, 1L), 111L)  # Rrii
