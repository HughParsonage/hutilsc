#include "hutilsc.h"

R_xlen_t sum_isna_ti1(const int * xp, R_xlen_t N) {
  R_xlen_t o = xp[0] == NA_INTEGER ? 1 : 0;
  for (R_xlen_t i = 1; i < N; ++i) {
    o += xp[i] == NA_INTEGER;
  }
  return o;
}


R_xlen_t sum_isna_tin(const int * xp, R_xlen_t N, int nThreads) {
  R_xlen_t o = xp[0] == NA_INTEGER ? 1 : 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(+ : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o += xp[i] == NA_INTEGER;
  }
  return o; 
}

SEXP do_sum_isna(SEXP x, SEXP nThread) {
  int nThreads = asInteger(nThread);
  if (TYPEOF(x) != INTSXP) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  R_xlen_t o = 0;
  if (nThreads > 1) {
    o += sum_isna_tin(xp, N, nThreads);
  } else {
    o += sum_isna_ti1(xp, N);
  }
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

