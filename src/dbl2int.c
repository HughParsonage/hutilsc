#include "hutilsc.h"


SEXP do_which_isnt_int(SEXP x) {
  if (TYPEOF(x) != REALSXP) {
    error("Internal error(do_ensure_int): TYPEOF(x) != REALSXP"); // # nocov
  }
  R_xlen_t N = xlength(x);
  const double * xp = REAL(x);
  // Just verify that all is ok
  R_xlen_t which_not_ok = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = xp[i];
    if (ISNAN(xi)) {
      continue;
    }
    if (xi > 2147483647 || xi < -2147483647) {
      which_not_ok = i + 1;
      break;
    }
    int ii = (int)xi;
    if (ii != xi) {
      which_not_ok = i + 1;
      break;
    }
  }
  return (which_not_ok < INT_MAX) ? ScalarInteger(which_not_ok) : ScalarReal(which_not_ok);
}
