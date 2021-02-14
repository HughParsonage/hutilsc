rid <- c("5002P00000DkX9tQAF", "5002P00000DqXJKQA3", "1750602", "5002P00000DqCtSQAV", 
         "2135143")

expect_equal(hutilsc:::DecodeRecordID(hutilsc:::EncodeRecordID(rid)), rid)

counts <- hutilsc:::CountRecordID(c("5002P00000DkX9tQAF", "5002P00000DkX9tQAF"))
expect_equal(counts, c(rep(1L, length(counts) - 1L), 0L))

counts2 <- hutilsc:::CountRecordID(c("5002P00000DkX9tQAF", "5002P00000DkX9tQAG"))
expect_equal(counts2, c(rep(1L, length(counts) - 2L), 2L, 0L))

counts3 <- hutilsc:::CountRecordID(c("5002P00000DkX9tQAF",
                                     "5002P00000DkX9tQAG",
                                     "5002P00000DkX9tQBZ"))
expect_equal(counts3, c(rep(1L, length(counts) - 3L), 2:3, 0L))


# Test non standard RecordID
counts4 <- hutilsc:::CountRecordID(c("5002P00000DkX9tQAF",
                                     "5002P00000DkX9tQAG",
                                     "5002P00000DkX9tQBZ",
                                     "Z"))
expect_equal(counts3, c(rep(1L, length(counts) - 3L), 2:3, 0L))

# expect_equal(hutilsc:::classify_chars(c("aA", "bb", "aa", "Za"), 2), c(15L, 15L))
# expect_equal(hutilsc:::classify_chars(c("aA", "bb", "aa", "Z0"), 2), c(15L, 30L))
# expect_equal(hutilsc:::classify_chars(c("aA", "bb", "aa", "Z0", "A0"), 2), c(15L, 30L))
