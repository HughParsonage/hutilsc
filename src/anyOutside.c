#include "hutilsc.h"

/*
static int type_safe2int(double x) {
  if (ISNAN(x)) {
    return 2;
  }
  if (x < -2147483647 || x > 2147483647) {
    return 0;
  }
  int xi = (int)x;
  return (xi == x) ? 1 : 0;
}
*/


static R_xlen_t do_anyOutside(SEXP x, SEXP a, SEXP b, SEXP skip_na) {
  if (TYPEOF(x) == NILSXP || (TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP)) {
    return 0;
  }
  const bool skip = asLogical(skip_na);
  R_xlen_t N = xlength(x);
  
  if (TYPEOF(x) == INTSXP && TYPEOF(a) == INTSXP && TYPEOF(b) == INTSXP) {
    const int *xp = INTEGER(x);
    const int app = asInteger(a);
    const int bpp = asInteger(b);
    if (skip) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] < app || xp[i] > bpp) {
          return i + 1;
        }
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] == NA_INTEGER || xp[i] < app || xp[i] > bpp) {
          return i + 1;
        }
      }
    }
  }
  if (TYPEOF(x) == REALSXP && TYPEOF(a) == REALSXP && TYPEOF(b) == REALSXP) {
    const double *xp = REAL(x);
    const int app = asReal(a);
    const int bpp = asReal(b);
    if (skip) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] < app || xp[i] > bpp) {
          return i + 1;
        }
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (ISNAN(xp[i]) || xp[i] < app || xp[i] > bpp) {
          return i + 1;
        }
      }
    }
  }
  if (TYPEOF(x) == INTSXP) {
    const int *xp = INTEGER(x);
    const double app_orig = asReal(a);
    const double bpp_orig = asReal(b);
    const double app_corr = ISNAN(app_orig) ? R_PosInf : app_orig;
    const double bpp_corr = ISNAN(bpp_orig) ? R_NegInf : bpp_orig;
    
    for (R_xlen_t i = 0; i < N; ++i) {
      double xpi = (double)(xp[i]);
      if (xpi < app_corr || xpi > bpp_corr) {
        return i + 1;
      }
    }
  }
  
  if (TYPEOF(x) == REALSXP) {
    const double *xp = REAL(x);
    const double app_orig = asReal(a);
    const double bpp_orig = asReal(b);
    const double app_corr = ISNAN(app_orig) ? R_PosInf : app_orig;
    const double bpp_corr = ISNAN(bpp_orig) ? R_NegInf : bpp_orig;
    
    for (R_xlen_t i = 0; i < N; ++i) {
      double xpi = (double)(xp[i]);
      if (xpi < app_corr || xpi > bpp_corr) {
        return i + 1;
      }
    }
  }
  
  return 0;
}

SEXP anyOutside(SEXP x, SEXP a, SEXP b, SEXP skip_na) {
  R_xlen_t ans = do_anyOutside(x, a, b, skip_na);
  if (ans < INT_MAX) {
    return ScalarInteger(ans);
  }
  return ScalarReal(ans);
}


SEXP test_amper(SEXP x) {
  int o = 1;
  int b = 1;
  b &= o;
  if (b) {
    Rprintf("yes ");
  } else {
    Rprintf("no ");
  }
  int s = 0;
  b &= s;
  if (b) {
    Rprintf("yes ");
  } else {
    Rprintf("no ");
  }
  return R_NilValue;
}




