#include "hutilsc.h"

unsigned int count_bits(int vi) {
  unsigned int v = vi;
  v = v - ((v >> 1U) & 0x55555555);                    // reuse input as temporary
  v = (v & 0x33333333) + ((v >> 2U) & 0x33333333);     // temp
  return (((v + (v >> 4U)) & 0xF0F0F0F) * 0x1010101) >> 24U; // count
}

// int sumpack1(int x) {
//   int o = 0;
//   while (x > 0) {
//     x >>= 1;
//     o += x & 1;
//   }
//   return o;
// }


SEXP sum_raw_from_lgl(SEXP x) {
  R_xlen_t N = xlength(x);
  const Rbyte * xp = RAW(x);
  int s = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned char xpi = xp[i];
    while (xpi) {
      s += xpi & 1;
      xpi >>= 1;
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = s;
  UNPROTECT(1);
  return ans;
}
SEXP sum_int_from_lgl(SEXP x) {
  R_xlen_t N = xlength(x);
  int s = 0;
  const int * xp = INTEGER(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    s += count_bits(xpi);
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = s;
  UNPROTECT(1);
  return ans;
}


SEXP do_packSum(SEXP x) {
  switch(TYPEOF(x)) {
  case RAWSXP: return sum_raw_from_lgl(x);
  case INTSXP: return sum_int_from_lgl(x);
  }
  return R_NilValue;
}



