#include "hutilsc.h"

SEXP CCountRaws(SEXP x, SEXP nthreads) {
  if (TYPEOF(x) != RAWSXP) {
    error("TYPEOF(x) != RAWSXP");
  }
  const unsigned char * xp = RAW(x);
  R_xlen_t N = xlength(x);
  unsigned int o[256] = {0};
  int nThread = as_nThread(nthreads);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = (unsigned char)xp[i];
    o[xi] += 1;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 256));
  int * ansp = INTEGER(ans);
  for (int i = 0; i < 256; ++i) {
    ansp[i] = o[i];
  }
  UNPROTECT(1);
  return ans;
}
