#include "hutilsc.h"


SEXP is_binary_call(SEXP sx) {
  if (TYPEOF(sx) != LANGSXP) {
    ScalarLogical(0);
  }
  // does it have three elements?
  int len = 0;
  
  /*
  for (SEXP nxt = sx; 
       nxt != R_NilValue || len > 4;
       SEXP el = CAR(nxt), nxt = CDR(nxt)) {
    len++;
  }
  
  */
  SEXP nxt = sx;
  while (len <= 4 && nxt != R_NilValue) {
    ++len;
    // SEXP el = CAR(nxt);
    nxt = CDR(nxt);
  }
  
  
  if (len != 3) {
    ScalarLogical(0);
  }
  
  if (TYPEOF(CAR(sx)) != SYMSXP) {
    ScalarLogical(0);
  }
  
  SEXP ans = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = TRUE;
  UNPROTECT(1);
  return ans;
}
