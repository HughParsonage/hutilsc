#include "hutilsc.h"

const char * Type(int type) {
  switch(type) {
  case NILSXP:
    return "NULL";
  case SYMSXP:
    return "symbol";
  case INTSXP:
    return "integer";
  case REALSXP:
    return "double";
  case LGLSXP:
    return "logical";
  }
  return "Unknown";
}

SEXP Ctypeof_from_int(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    error("x must be an integer (TYPEOF)");
  }
  const char * Typ = Type(asInteger(x));
  return ScalarString(mkCharCE(Typ, CE_UTF8));
}
