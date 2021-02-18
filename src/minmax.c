#include "hutilsc.h"

int maxii(int x, int y) {
  return (x >= y) ? x : y;
}

int maxX(const int * x, R_xlen_t N, bool sx) {
  if (N == 0) {
    return INT_MIN;
  }
  if (sx) {
    return x[N - 1];
  }
  int max_x = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    max_x = maxii(max_x, x[i]);
  }
  return max_x;
}



int maxXY(const int * x, const int * y, R_xlen_t Nx, R_xlen_t Ny, bool sx, bool sy) {
  if (Nx == 0) {
    
  }
  if (sx & sy) {
    return maxii(x[Nx - 1], y[Ny - 1]);
  }
  if (sx) {
    int max_xy = x[Nx - 1];
    for (R_xlen_t i = 0; i < Ny; ++i) {
      max_xy = maxii(max_xy, y[i]);
    }
    return max_xy;
  }
  if (sy) {
    int max_xy = y[Nx - 1];
    for (R_xlen_t i = 0; i < Nx; ++i) {
      max_xy = maxii(max_xy, x[i]);
    }
    return max_xy;
  }
  return maxii(maxX(x, Nx, sx), maxX(y, Ny, sy));
}


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

void do_whichminmax_lgl(const int * x, R_xlen_t N,
                        R_xlen_t * ansp) {
  R_xlen_t wmin = 0;
  R_xlen_t wmax = 0;
  R_xlen_t i = 0;
  
  // Stop when x[wmin] = 0
  // Stop when x[xmax] = 1;
  // Continue when x[i] is NA
  bool search4wmin = (x[0] != 0);
  bool search4wmax = (x[0] != 0);
  
  while (++i < N && search4wmin && search4wmax) {
    int xi = x[i];
    if (xi == 1) {
      wmax = i;
      search4wmax = false;
    } else if (xi == 0) {
      wmin = i;
      search4wmin = false;
    }
  }
  ansp[0] = wmin;
  ansp[1] = wmax;
}

void do_whichminmax_int(const int x[], R_xlen_t N, R_xlen_t * ansp) {
  R_xlen_t wmin = 0, wmax = 0;
  int xmin = x[0];
  int xmax = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    int xi = x[i];
    if (xi < xmin) {
      xmin = xi;
      wmin = i;
    } else if (xi > xmax) {
      xmax = xi;
      wmax = i;
    }
  }
  ansp[0] = wmin;
  ansp[1] = wmax;
}

void do_whichminmax_dbl(const double * x, R_xlen_t N, R_xlen_t * ansp) {
  R_xlen_t wmin = 0, wmax = 0;
  double xmin = x[0];
  double xmax = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    double xi = x[i];
    if (xi < xmin) {
      xmin = xi;
      wmin = i;
    } else if (xi > xmax) {
      xmax = xi;
      wmax = i;
    }
  }
  ansp[0] = wmin;
  ansp[1] = wmax;
}

SEXP do_whichminmax(SEXP x) {
  
  R_xlen_t N = xlength(x);
  // Choose N - 1 since we want the minimum that satisfies.
  R_xlen_t ansp[2] = {-1, -1};
  
  if (TYPEOF(x) == LGLSXP) {
    const int * xp = INTEGER(x);
    do_whichminmax_lgl(xp, N, ansp);
  } else if (TYPEOF(x) == INTSXP) {
    const int * xp = INTEGER(x);
    do_whichminmax_int(xp, N, ansp);
  } else if (TYPEOF(x) == REALSXP) {
    const double * xp = REAL(x);
    do_whichminmax_dbl(xp, N, ansp);
  } else if (TYPEOF(x) == STRSXP) {
    const char * xi0 = CHAR(STRING_ELT(x, 0));
    R_xlen_t wmin = 0, wmax = 0;
    const char * xmin = xi0;
    const char * xmax = xi0;
    for (R_xlen_t i = 1; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      if (strcmp(xi, xmin) < 0) {
        xmin = xi;
        wmin = i;
      } else if (strcmp(xi, xmax) > 0) {
        xmax = xi;
        wmax = i;
      }
    }
    ansp[0] = wmin;
    ansp[1] = wmax;
  }
  if (ansp[0] > INT_MAX || ansp[1] > INT_MAX) {
    SEXP ans = PROTECT(allocVector(REALSXP, 2));
    REAL(ans)[0] = ansp[0];
    REAL(ans)[1] = ansp[1];
    UNPROTECT(1);
    return ans;
  } else {
    SEXP ans = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ans)[0] = ansp[0];
    INTEGER(ans)[1] = ansp[1];
    UNPROTECT(1);
    return ans;
  }
  
  return R_NilValue;
}


