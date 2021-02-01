#include "hutilsc.h"



void collatz(int * seq, size_t * pp, size_t n) {
  size_t p = *pp;
  if (p + 4 > n) {
    ++p;
    return;
  }
  
  Rprintf("_%d_,_%d_", (int)p, (int)n);
  
  if (seq[p] == 4) {
    p++;
    seq[p] = 2;
    p++;
    seq[p] = 1;
    *pp = p;
    return;
  }
  if (seq[p] == 2) {
    p++;
    seq[p] = 1;
    *pp = p;
    return;
  }
  if (seq[p] == 1) {
    return;
  }
  if (p * 2 > n) {
    Rprintf("\n%d req:\t", (int)(p * 2 * sizeof(int)));
    n = p * 2;
  }
  int seqp = seq[p];
  if ((seqp & 1)) {
    // odd
    ++p;
    seq[p] = 3 * seqp + 1;
    *pp = p;
  } else {
    ++p;
    seq[p] = seqp / 2;
    *pp = p;
  }
}


SEXP do_collatz(SEXP ss) {
  int64_t s = asInteger(ss);
  int64_t * seq = malloc(1000 * sizeof(int64_t));
  if (seq == NULL) {
    error("Unable");
  }
  seq[0] = s;
  R_xlen_t p = 0;
  bool exceeds_int_max = s < INT_MAX;
  while (p < 1000 && seq[p] > 1) {
    int64_t seqp = seq[p];
    exceeds_int_max |= seqp > INT_MAX;
    if (seqp & 1) {
      ++p;
      seq[p] = 3 * seqp + 1;
    } else {
      ++p;
      seq[p] = seqp / 2;
    }
  }
  if (exceeds_int_max) {
    SEXP ans = PROTECT(allocVector(REALSXP, p + 1));
    double * ansp = REAL(ans);
    for (R_xlen_t i = 0; i < (p + 1); ++i) {
      ansp[i] = seq[i];
    }
    free(seq);
    UNPROTECT(1);
    return ans;
  } 
  SEXP ans = PROTECT(allocVector(INTSXP, p + 1));
  int * ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < (p + 1); ++i) {
    ansp[i] = seq[i];
  }
  free(seq);
  UNPROTECT(1);
  return ans;
}


