

Ctest_input_types <- function(x1, y1, x2, y2) {
  .Call("test_input_types", x1, y1, x2, y2, PACKAGE = packageName())
}
