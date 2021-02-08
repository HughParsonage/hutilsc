rid <- c("5002P00000DkX9tQAF", "5002P00000DqXJKQA3", "1750602", "5002P00000DqCtSQAV", 
         "2135143")

expect_equal(hutilsc:::DecodeRecordID(hutilsc:::EncodeRecordID(rid)), rid)
