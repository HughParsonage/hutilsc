#include "hutilsc.h"

SEXP do_sum_isna(SEXP x, SEXP nThread) {
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
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

