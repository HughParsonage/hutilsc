// Functions to record how to perform some known tasks

#include "hutilsc.h"


SEXP Ccollatz(SEXP ss) {
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



SEXP Cfibonacci(SEXP nn, SEXP return_seq) {
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

SEXP Callocate0_int(SEXP N, SEXP nThread) {
  if (xlength(N) != 1 || (TYPEOF(N) != INTSXP && TYPEOF(N) != REALSXP)) {
    error("N not a single number.");
  }
  if (xlength(nThread) != 1 ||
      (TYPEOF(nThread) != INTSXP && TYPEOF(nThread) != REALSXP)) {
    error("nThread not a single number.");
  }
  R_xlen_t n = TYPEOF(N) == INTSXP ? asInteger(N) : asReal(N);
  int nthreads = asInteger(nThread);
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * restrict out = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthreads)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    out[i] = 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cevery_int32(SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(INTSXP, 4294967296));
  int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (unsigned int i = 0; i < 4294967295; ++i) {
    ansp[i] = i;
  }
  ansp[4294967295] = -1;
  UNPROTECT(1);
  return ans;
}

