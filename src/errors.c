#include "hutilsc.h"


bool notInt(SEXP x) {
  return TYPEOF(x) != INTSXP || xlength(x) != 1;
}

bool notDbl(SEXP x) {
  return TYPEOF(x) != REALSXP || xlength(x) != 1;
}

#define ERR_XNOT_LGL 1011289981
#define ERR_XNOT_INT 1586477427
#define ERR_YNOT_LGL 1011821422
#define ERR_YNOT_INT 1587008868
#define ERR_ZNOT_INT 1587540309
#define ERR_XNOT_DBL -1592347128
#define ERR_YNOT_DBL -1591815687
#define ERR_ZNOT_DBL -1591284246
#define ERR_UNEQ_LEN -873554869
#define ERR_LONG_LEN 1098478544

int notEquiLgl2(SEXP x, SEXP y) {
  if (TYPEOF(x) != LGLSXP) {
    return ERR_XNOT_LGL;
  }
  if (TYPEOF(y) != LGLSXP) {
    return ERR_YNOT_LGL;
  }
  if (xlength(x) != xlength(y)) {
    return ERR_UNEQ_LEN;
  }
  if (xlength(x) >= INT_MAX) {
    return ERR_LONG_LEN;
  }
  return 0;
}

int notEquiInt2(SEXP x, SEXP y) {
  if (TYPEOF(x) != INTSXP) {
    return ERR_XNOT_INT;
  }
  if (TYPEOF(y) != INTSXP) {
    return ERR_YNOT_INT;
  }
  if (xlength(x) != xlength(y)) {
    return ERR_UNEQ_LEN;
  }
  if (xlength(x) >= INT_MAX) {
    return ERR_LONG_LEN;
  }
  return 0;
}

int notEquiInt3(SEXP x, SEXP y, SEXP z) {
  if (TYPEOF(x) != INTSXP) {
    return ERR_XNOT_INT;
  }
  if (TYPEOF(y) != INTSXP) {
    return ERR_YNOT_INT;
  }
  if (TYPEOF(z) != INTSXP) {
    return ERR_ZNOT_INT;
  }
  if (xlength(x) != xlength(y)) {
    return ERR_UNEQ_LEN;
  }
  if (xlength(x) != xlength(z)) {
    return ERR_UNEQ_LEN;
  }
  if (xlength(x) >= INT_MAX) {
    return ERR_LONG_LEN;
  }
  return 0;
}

int notEquiDbl2(SEXP x, SEXP y) {
  if (TYPEOF(x) != REALSXP) {
    return ERR_XNOT_DBL;
  }
  if (TYPEOF(y) != REALSXP) {
    return ERR_YNOT_DBL;
  }
  if (xlength(x) != xlength(y)) {
    return ERR_UNEQ_LEN;
  }
  if (xlength(x) >= INT_MAX) {
    return ERR_LONG_LEN;
  }
  return 0;
}

int notEquiDbl3(SEXP x, SEXP y, SEXP z) {
  int xy = notEquiDbl2(x, y);
  int xz = notEquiDbl2(x, z);
  if (xy) return xy;
  return xz;
}





#define ERR_EPI_NOT_LIST 1379605085;

int ValidateEpi(SEXP Epi) {
  if (TYPEOF(Epi) != VECSXP) {
    return ERR_EPI_NOT_LIST;
  }
  //if (getAttrib(Epi, ))
  return 0;
}