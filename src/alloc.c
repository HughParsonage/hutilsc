// Functions to record how to perform some known tasks

#include "hutilsc.h"


SEXP do_collatz(SEXP ss) {
  int64_t s = asInteger(ss);
  int64_t * seq = malloc(1000 * sizeof(int64_t));
  if (seq == NULL) {
    error("Unable");
  }
  seq[0] = s;
  R_xlen_t p = 0;
  bool exceeds_int_max = s > INT_MAX;
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



SEXP do_fibonacci(SEXP nn, SEXP return_seq) {
  int n = asInteger(nn);
  // const bool retSeq = asLogical(return_seq);
  if (n == 1) {
    return ScalarInteger(1);
  }
  if (n == 2) {
    SEXP ans = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ans)[0] = 1;
    INTEGER(ans)[1] = 1;
    UNPROTECT(1);
    return ans;
  }
  if (n > 1024) {
    return R_NilValue;
  }
  R_xlen_t s[n];
  s[0] = 1;
  s[1] = 1;
  bool exceed_int_max = false;
  for (R_xlen_t i = 2; i < n; ++i) {
    s[i] = s[i - 1] + s[i - 2];
    exceed_int_max = s[i] > INT_MAX;
  }
  if (exceed_int_max) {
    SEXP ans = PROTECT(allocVector(REALSXP, n));
    double * restrict ansp = REAL(ans);
    for (R_xlen_t i = 0; i < n; ++i) {
      ansp[i] = s[i];
    }
    UNPROTECT(1);
    return ans;
  } 
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < n; ++i) {
    ansp[i] = s[i];
  }
  UNPROTECT(1);
  return ans;
} 


