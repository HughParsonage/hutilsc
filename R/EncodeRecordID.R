

EncodeRecordID <- function(x) {
  .Call("do_encodeRecordID", x, PACKAGE = packageName())
}
