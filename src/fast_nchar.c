#include "hutilsc.h"

int int_log2(uint32_t x) { return 31 - __builtin_clz(x|1); }

int fast_digit_count(uint32_t x) {
  static uint64_t table[] = {
    4294967296,  8589934582,  8589934582,  8589934582,  12884901788,
    12884901788, 12884901788, 17179868184, 17179868184, 17179868184,
    21474826480, 21474826480, 21474826480, 21474826480, 25769703776,
    25769703776, 25769703776, 30063771072, 30063771072, 30063771072,
    34349738368, 34349738368, 34349738368, 34349738368, 38554705664,
    38554705664, 38554705664, 41949672960, 41949672960, 41949672960,
    42949672960, 42949672960};
  return (x + table[int_log2(x)]) >> 32;
}


SEXP Cfast_nchar(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = fast_digit_count(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}
