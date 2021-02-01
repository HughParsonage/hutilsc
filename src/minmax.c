#include "hutilsc.h"

SEXP do_minmax(SEXP x, SEXP emptyResult, SEXP nThread) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return emptyResult;
  }
  int nthreads = asInteger(nThread);
  
  switch(TYPEOF(x)) {
  case INTSXP: {
    const int *xp = INTEGER(x);
    int xmin = xp[0];
    int xmax = xp[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthreads) reduction(min : xmin) reduction(max : xmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      int xi = xp[i];
      bool nochange = xi >= xmin && xi <= xmax;
      if (nochange) continue;
      xmin = (xi < xmin) ? xi : xmin;
      xmax = (xi > xmax) ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ans)[0] = xmin;
    INTEGER(ans)[1] = xmax;
    UNPROTECT(1);
    return ans;
  }
    break;
  case REALSXP: {
    const double *xp = REAL(x);
    double xmin = xp[0];
    double xmax = xp[0];
    if (ISNAN(xmin)) {
      xmin = R_PosInf;
    }
    if (ISNAN(xmax)) {
      xmax = R_NegInf;
    }
    
    for (R_xlen_t i = 1; i < N; ++i) {
      double xi = xp[i];
      bool nochange = xi >= xmin && xi <= xmax;
      if (nochange || ISNAN(xi)) continue;
      xmin = (xi < xmin) ? xi : xmin;
      xmax = (xi > xmax) ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(REALSXP, 2));
    REAL(ans)[0] = xmin;
    REAL(ans)[1] = xmax;
    UNPROTECT(1);
    return ans;
  }
    break;
  case STRSXP: {
    const char * xmin = CHAR(STRING_ELT(x, 0));
    const char * xmax = CHAR(STRING_ELT(x, 0));
    for (R_xlen_t i = 1; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      xmin = strcmp(xi, xmin) < 0 ? xi : xmin;
      xmax = strcmp(xi, xmax) > 0 ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(ans, 0, Rf_mkChar(xmin));
    SET_STRING_ELT(ans, 1, Rf_mkChar(xmax));
    UNPROTECT(1);
    return ans;
  }
    break;
    
  }
  return R_NilValue;
}
