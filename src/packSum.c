#include "hutilsc.h"
#include <popcntintrin.h>
#include <xmmintrin.h>

unsigned int count_bits(int vi) {
  unsigned int v = vi;
  v = v - ((v >> 1U) & 0x55555555);                    // reuse input as temporary
  v = (v & 0x33333333) + ((v >> 2U) & 0x33333333);     // temp
  return (((v + (v >> 4U)) & 0xF0F0F0F) * 0x1010101) >> 24U; // count
}

int popcountr(unsigned int a) {
  // return _mm_popcnt_u32(a);
  return count_bits((int)a);
  //return __popcnt(a);
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
SEXP sum_int_from_lgl(SEXP x, SEXP m) {
  R_xlen_t N = xlength(x);
  const int M = asInteger(m);
  int s = 0;
  const int * xp = INTEGER(x);
  if (M) {
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned int xpi = xp[i];
      s += (unsigned int)popcountr(xpi);
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      int xpi = xp[i];
      s += count_bits(xpi);
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = s;
  UNPROTECT(1);
  return ans;
}


SEXP CpackSum(SEXP x, SEXP m) {
  switch(TYPEOF(x)) {
  case RAWSXP: return sum_raw_from_lgl(x);
  case INTSXP: return sum_int_from_lgl(x, m);
  }
  return R_NilValue;
}

// From R source (packBits)

SEXP do_packBits(SEXP x, SEXP use_raw) {
  Rboolean useRaw;
  R_xlen_t i, len = XLENGTH(x), slen;
  int fac;
  const bool xIsRaw = TYPEOF(x) == RAWSXP;
  if (TYPEOF(x) != RAWSXP && TYPEOF(x) != LGLSXP && TYPEOF(x) != INTSXP) {
    error("argument 'x' must be raw, integer or logical");
  }
  useRaw = asLogical(use_raw);
  fac = useRaw ? 8 : 32;
  if (len % fac) {
    error("argument 'x' must be a multiple of %d long", fac);
  }
  slen = len/fac;
  SEXP ans = PROTECT(allocVector(useRaw ? RAWSXP : INTSXP, slen));
  for (i = 0; i < slen; i++) {
    if (useRaw) {
      Rbyte btmp = 0;
      for (int k = 7; k >= 0; k--) {
        btmp <<= 1;
        if (xIsRaw)
          btmp |= RAW(x)[8*i + k] & 0x1;
        else if (isLogical(x) || isInteger(x)) {
          int j = INTEGER(x)[8*i+k];
          if (j == NA_INTEGER)
            error("argument 'x' must not contain NAs");
          btmp |= j & 0x1;
        }
      }
      RAW(ans)[i] = btmp;
    } else {
      unsigned int itmp = 0;
      for (int k = 31; k >= 0; k--) {
        itmp <<= 1;
        if (xIsRaw)
          itmp |= RAW(x)[32*i + k] & 0x1;
        else if (isLogical(x) || isInteger(x)) {
          int j = INTEGER(x)[32*i+k];
          if (j == NA_INTEGER)
            error("argument 'x' must not contain NAs");
          itmp |= j & 0x1;
        }
      }
      INTEGER(ans)[i] = (int) itmp;
    }
  }
  UNPROTECT(1);
  return ans;
}




