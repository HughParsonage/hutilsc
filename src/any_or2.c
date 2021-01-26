#include "hutilsc.h"

SEXP any_or2(SEXP x, SEXP b, SEXP y) {
  R_xlen_t N = xlength(x);
  if (xlength(y) != N || xlength(b) != 1) {
    error("Wrong lengths.");
  }
  const int *xp = INTEGER(x);
  const int *yp = INTEGER(y);
  const int *bp = INTEGER(b);
  
  bool o = false;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] >= bp[0] || xp[i] == yp[i]) {
      o = true;
      break;
    }
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = o ? TRUE : FALSE;
  UNPROTECT(1);
  return ans;
}

SEXP typeof_int(SEXP x) {
  return ScalarInteger(TYPEOF(x));
}

SEXP complete_cases(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP) {
    error("Not integer.");
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int *restrict ansp = LOGICAL(ans);
  const int *xp = INTEGER(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = xp[i] != NA_INTEGER;
  }
  UNPROTECT(1);
  return ans;
}
