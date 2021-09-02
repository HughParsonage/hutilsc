expect_equal(pad0("abc"), "abc")
expect_equal(pad0(c("abc", "abcd")), c("0abc", "abcd"))
expect_equal(pad0("1234", 5), "01234")
expect_equal(pad0(c("1234", "123", "12345"), 4L), c("1234", "0123", "12345"))

