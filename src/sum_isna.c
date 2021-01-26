#include "hutilsc.h"

SEXP sum_isna(SEXP x, SEXP nThread) {
  int nThreads = asInteger(nThread);
  R_xlen_t o = 0;
  
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o += xp[i] == NA_INTEGER;
  }
  if (o < INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, 1));
    INTEGER(ans)[0] = o;
    UNPROTECT(1);
    return ans;
  } else {
    SEXP ans = PROTECT(allocVector(REALSXP, 1));
    REAL(ans)[0] = o;
    UNPROTECT(1);
    return ans;
  }
}

