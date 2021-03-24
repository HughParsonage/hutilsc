

EncodeRecordID <- function(x) {
  .Call("CencodeRecordID", x, PACKAGE = packageName())
}
