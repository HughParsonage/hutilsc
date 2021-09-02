#include "hutilsc.h"

bool is_constant_int(const int * x, int nThreads, const R_xlen_t N) {
  const int x0 = x[0];
  // faster when x isn't constant, not much slower otherwise
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

bool is_constant_dbl(const double * x, int nThreads, const R_xlen_t N) {
  const double x0 = x[0];
  // faster when x isn't constant, not much slower otherwise
  if (nThreads == 1) {
    for (R_xlen_t i = 1; i < N; ++i) {
      if (x[i] != x0) {
        return false;
      }
    }
    return true;
  } else {
    
    char o = 1;
    if (ISNAN(x0)) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
      for (R_xlen_t i = 1; i < N; ++i) {
        o &= ISNAN(x[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
      for (R_xlen_t i = 1; i < N; ++i) {
        o &= x[i] == x0;
      }
    }
    return o ? true : false;
  }
  return false;
}


SEXP Cis_constant(SEXP x, SEXP nThread) {
  R_xlen_t N = xlength(x);
  if (N < 2) {
    return ScalarLogical(1);
  }
  int nThreads = asInteger(nThread);
  switch(TYPEOF(x)) {
  case NILSXP:
    return ScalarLogical(1);
    break;
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    if (is_constant_int(xp, nThreads, N)) {
      return ScalarLogical(1);
    } else {
      return ScalarLogical(0);
    }
  }
    break;
    
  case REALSXP: {
    const double * xp = REAL(x);
    if (is_constant_dbl(xp, nThreads, N)) {
      return ScalarLogical(1);
    } else {
      return ScalarLogical(0);
    }
  }
    break;
  case CPLXSXP: {
    const Rcomplex * xp = COMPLEX(x);
    const Rcomplex xp0 = xp[0];
    const double R0 = xp0.r;
    const double I0 = xp0.i;
    for (R_xlen_t i = 1; i < N; ++i) {
      Rcomplex xpi = xp[i];
      double Rxi = xpi.r;
      double Ixi = xpi.i;
      if (Rxi != R0 || Ixi != I0) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
    break;
  case STRSXP: {
    const char * x0 = CHAR(asChar(x));
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      if (xi != x0) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
  case RAWSXP: {
    
    const Rbyte * xp = RAW(x);
    const Rbyte x0 = xp[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i] != x0) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
  }
  return R_NilValue;
}

SEXP Crepe(SEXP x, SEXP y, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (notEquiInt2(x, y)) {
    error("notEquiInt2(x, y)");
  }
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  int nThread = as_nThread(nthreads);
  R_xlen_t NN = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : NN)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    NN += yp[i];
  }
  if (NN >= INT_MAX) {
    error("NN >= INT_MAX");
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, NN));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0, k = 0; i < N; ++i) {
    int ni = yp[i];
    int xi = xp[i];
    for (int j = 0; j < ni; ++j) {
      ansp[k++] = xi;
    }
  }
  UNPROTECT(1);
  return ans;
} 





