#include "hutilsc.h"

R_xlen_t which_isnt_sorted(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return 0;
  }
  R_xlen_t o = 0;
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i - 1] > xp[i]) {
        o = i; 
        break;
      }
    }
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i - 1] > xp[i]) {
        o = i; 
        break;
      }
    }
  }
    break;
  case STRSXP: {
    
    for (R_xlen_t i = 1; i < N; ++i) {
    const char * xpi0 = CHAR(STRING_ELT(x, i - 1));
    const char * xpi1 = CHAR(STRING_ELT(x, i));
    int s = strcmp(xpi0, xpi1);
      if (s > 0) {
        o = i; 
        break;
      }
    }
  }
    break;
    
    
  }
  return o;
}

bool sorted_int(const int * xp, R_xlen_t N, int nThreads) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= xp[i - 1] <= xp[i]; 
  }
  return (bool)o;
}

bool sorted_dbl(const double * xp, R_xlen_t N, int nThreads) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= xp[i - 1] <= xp[i]; 
  }
  return (bool)o;
}

bool sorted_str(SEXP x, R_xlen_t N, int nThreads) {
  char o = 1;
  for (R_xlen_t i = 1; i < N; ++i) {
    const char * xi0 = CHAR(STRING_ELT(x, i - 1));
    const char * xi1 = CHAR(STRING_ELT(x, i));
    int s = strcmp(xi0, xi1);
    o &= s <= 0;
  }
  return (bool)o;
}


SEXP do_which_isnt_sorted(SEXP x) {
  R_xlen_t o = which_isnt_sorted(x);
  return o <= INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

SEXP do_is_sorted(SEXP x, SEXP nThread) {
  int nThreads = asInteger(nThread);
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return_true;
  }
  
  char o = -1;
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    o = sorted_int(xp, N, nThreads);
    return ScalarLogical(o);
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    o = sorted_dbl(xp, N, nThreads);
  }
    break;
  case STRSXP: {
    o = sorted_str(x, N, nThreads);
  }
    
    
  }
  if (o < 0) {
    return R_NilValue;
  }
  return ScalarLogical((bool)o);
}

