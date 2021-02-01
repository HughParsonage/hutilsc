#include "hutilsc.h"
 
#define for_weq(EXPR)                                          \
for (int i = 0; i < n; ++i) {                                  \
  if (EXPR) {                                                  \
    k = i + 1; break;                                          \
  }                                                            \
}



SEXP do_which_first_eq(SEXP x, SEXP o, SEXP a) {
  int n = length(x);
  const int *xp = INTEGER(x);
  const int opp = asInteger(o);
  int k = 0;
  const int aa = asInteger(a);   
  for (int i = 0; i < n; ++i) {
    if (xp[i] == aa) {
      k = i + 1;
      break;
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = k;
  UNPROTECT(1);
  return ans;
}



SEXP do_which_first(SEXP x, SEXP o, SEXP a) {
  R_xlen_t N = xlength(x);
  if (N >= INT_MAX) {
    error("N >= INT_MAX");
  }
  int k = 0;
  int n = (int)N;
  
  const int *xp = INTEGER(x);
  const int opp = asInteger(o);
  
  const int aa = asInteger(a);
  switch(opp) {
  case OP_NE:
    for (int i = 0; i < n; ++i) {
      if (xp[i] != aa) {
        k = i + 1;
        break;
      }
    }
    break;
  case OP_EQ:
    for (int i = 0; i < n; ++i) {
      if (xp[i] == aa) {
        k = i + 1;
        break;
      }
    }
    break;
  case OP_GE:
    for (int i = 0; i < n; ++i) {
      if (xp[i] >= aa) {
        k = i + 1;
        break;
      }
    }
    break;
  case OP_LE:
    for_weq(xp[i] <= aa)
    break;
  case OP_GT:
    for_weq(xp[i] > aa)
    break;
  case OP_LT:
    for_weq(xp[i] < aa)
    break;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = k;
  UNPROTECT(1);
  return ans;
}





