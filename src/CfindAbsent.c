#include "hutilsc.h"


SEXP CfindAbsent(SEXP x, SEXP nthreads) {
  // Given x \subset {1, 2, ..., N} with 
  // {1, 2, ..., N} \ x == {i}, return i 
  if (TYPEOF(x) != INTSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  int nThread = as_nThread(nthreads);
  int ans = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(^ : ans)
#endif
  for (R_xlen_t i = 0; i <= N; ++i) {
    ans ^= ((unsigned int)(i + 1u));
  }
  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(^ : ans)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ans ^= xp[i];
  }
  return ScalarInteger(ans);
}


