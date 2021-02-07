#include "hutilsc.h"

bool is_constant_int(const int * x, int nThreads, const R_xlen_t N) {
  const int x0 = x[0];
  if (nThreads == 1) {
    for (R_xlen_t i = 1; i < N; ++i) {
      if (x[i] != x0) {
        return false;
      }
    }
    return true;
  } else {
    char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
for (R_xlen_t i = 1; i < N; ++i) {
  o &= x[i] == x0;
}
return o ? true : false;
  }
  return false;
}


SEXP do_is_constant(SEXP x, SEXP nThread) {
  const R_xlen_t N = xlength(x);
  if (N <= 1) {
    return_true;
  }
  int nThreads = asInteger(nThread);
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    bool o = is_constant_int(xp, nThreads, N);
    return ScalarLogical(o);
  }
    
  }
  return R_NilValue;
}

