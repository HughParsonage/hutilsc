#include "hutilsc.h"

SEXP CSumRaw(SEXP x, SEXP nthreads) {
  if (TYPEOF(x) != RAWSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  const unsigned char * xp = RAW(x);
  R_xlen_t o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o += (int)xp[i];
  }
  return ScalarLength(o);
}
