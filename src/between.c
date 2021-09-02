#include "hutilsc.h"

SEXP do_between_lgl(SEXP x, SEXP lwr, SEXP upr) {
  R_xlen_t N = xlength(x);
  const int a = asLogical(lwr);
  const int b = asLogical(upr);
  
  if (xlength(lwr) == 0 || xlength(upr) == 0) {
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    UNPROTECT(1);
    return ans;
  }
  const int * xp = LOGICAL(x);
  
  if (xlength(lwr) == 1 && xlength(upr) == 1) {
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    int *restrict ansp = LOGICAL(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      int xpi = xp[i];
      ansp[i] = xpi >= a && xpi <= b;
    }
    UNPROTECT(1);
    return ans;
  }
  if (xlength(lwr) == N && xlength(upr) == N) {
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    int *restrict ansp = LOGICAL(ans);
    const int *ap = LOGICAL(lwr);
    const int *bp = LOGICAL(upr);
    for (R_xlen_t i = 0; i < N; ++i) {
      int xpi = xp[i];
      ansp[i] = xpi >= ap[i] && xpi <= bp[i];
    }
    UNPROTECT(1);
    return ans;
  }
  return R_NilValue;
}

SEXP do_between_int(SEXP x, SEXP lwr, SEXP upr, SEXP nthreads) {
  const int a = asInteger(lwr);
  const int b = asInteger(upr) == NA_INTEGER ? INT_MAX : asInteger(upr);
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
  if (b < a) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
    }
    UNPROTECT(1);
    return ans;
  }
  const int * xp = INTEGER(x);
  const unsigned int b_minus_a = (unsigned int)b - a;
  const unsigned int au = (unsigned int)a;
  pragma_omp
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (unsigned int)(xp[i] - au) <= b_minus_a;
  }
  UNPROTECT(1);
  return ans;
}

SEXP do_between(SEXP x, SEXP lwr, SEXP upr, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return R_NilValue;
  }
  switch(TYPEOF(x)) {
  case LGLSXP:
    return do_between_lgl(x, lwr, upr);
  case INTSXP:
    return do_between_int(x, lwr, upr, nthreads);
    /*
  case REALSXP:
    return do_between_dbl(x, lwr, upr);
  case STRSXP:
    return do_between_str(x, lwr, upr);
     */
  }
  return R_NilValue;
}


