#include "hutilsc.h"

SEXP is_constant(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N < 2) {
    return_true;
  }
  switch(TYPEOF(x)) {
  case NILSXP:
    return_true;
    break;
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    const int xp0 = xp[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i] != xp0) {
        return_false;
      }
    }
    return_true;
  }
  case REALSXP: {
    const double * xp = REAL(x);
    const double xp0 = xp[0];
    if (ISNAN(xp0)) {
      for (R_xlen_t i = 1; i < N; ++i) {
        if (!ISNAN(xp[i])) {
          return_false;
        }
      }
    } else {
      for (R_xlen_t i = 1; i < N; ++i) {
        if (xp[i] != xp0) {
          return_false;
        }
      }
    }
    return_true;
  }
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
        return_false;
      }
    }
    return_true;
  }
  case STRSXP: {
    const char * x0 = CHAR(asChar(x));
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      if (xi != x0) {
        return_false;
      }
    }
    return_true;
  }
  case RAWSXP: {
    
    const Rbyte * xp = RAW(x);
    const Rbyte x0 = xp[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i] != x0) {
        return_false;
      }
    }
    return_true;
  }
  }
  return R_NilValue;
}




