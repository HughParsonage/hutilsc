#include "hutilsc.h"

int pmax0_int_(int x) {
  return (x > 0) ? x : 0;
}

SEXP Cpmax0_int(SEXP x) {
  R_xlen_t N = xlength(x);
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = pmax0_int_(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cpmax0_dbl(SEXP x) {
  R_xlen_t N = xlength(x);
  const double *xp = REAL(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double *restrict ansp = REAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (xp[i] >= 0) ? xp[i] : 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cpmax0(SEXP x) {
  switch(TYPEOF(x)) {
  case INTSXP:
    return Cpmax0_int(x);
    break;
  case REALSXP:
    return Cpmax0_dbl(x);
  }
  error("Invalid typeof(x)");
  return x;
}

