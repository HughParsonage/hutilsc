#include "hutilsc.h"

R_xlen_t sum_isna_ti1(const int * xp, R_xlen_t N) {
  R_xlen_t o = xp[0] == NA_INTEGER ? 1 : 0;
  for (R_xlen_t i = 1; i < N; ++i) {
    o += xp[i] == NA_INTEGER;
  }
  return o;
}


R_xlen_t sum_isna_tin(const int * xp, R_xlen_t N, int nThreads) {
  R_xlen_t o = xp[0] == NA_INTEGER ? 1 : 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(+ : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o += xp[i] == NA_INTEGER;
  }
  return o; 
}

SEXP Csum_isna(SEXP x, SEXP nThread) {
  int nThreads = asInteger(nThread);
  if (TYPEOF(x) != INTSXP) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  R_xlen_t o = 0;
  if (nThreads > 1) {
    o += sum_isna_tin(xp, N, nThreads);
  } else {
    o += sum_isna_ti1(xp, N);
  }
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}


SEXP do_which_na(SEXP x, SEXP Not) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP) {
    return R_NilValue;
  }
  const int * xp = INTEGER(x);
  if (N < 128) {
    int * idx = malloc(sizeof(int) * N);
    if (idx == NULL) {
      free(idx);
      return R_NilValue;
    }
    int k = 0;
    int i = 0;
    while (i < N) {
      int isna = xp[i] == NA_INTEGER;
      idx[k] = i + 1;
      k += isna;
      ++i;
    }
    if (k == 0) {
      free(idx);
      return allocVector(INTSXP, 0);
    }
    SEXP ans = PROTECT(allocVector(INTSXP, k));
    int * restrict ansp = INTEGER(ans);
    for (R_xlen_t j = 0; j < k; ++j) {
      ansp[j] = idx[j];
    }
    free(idx);
    UNPROTECT(1);
    return ans;
  }
  int e = 1;
  for (int i = 0; i < 128; ++i) {
    e += xp[i] == NA_INTEGER;
  }
  if (e == 129) {
    --e;
  }
  double ep = ((double)e) / 128.0;
  R_xlen_t M = N * ep;
  int * idx = malloc(sizeof(int) * M);
  
  int n_na = 0;
  int k = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    bool isna = xp[i] == NA_INTEGER;
    if (!(i & 127)) {
      if (M <= n_na - 2) {
        M *= 2;
        idx = realloc(idx, M * sizeof(int));
        if (idx == NULL) {
          free(idx);
          return R_NilValue;
        }
      }
    }
    idx[k] = i;
    k += isna;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, k));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t j = 0; j < k; ++j) {
    ansp[j] = idx[j];
  }
  free(idx);
  UNPROTECT(1);
  return ans;
}

SEXP Csum_int(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    return ScalarInteger(0);
  }
  const int * xp = INTEGER(x);
  R_xlen_t N = xlength(x);
  uint64_t o = 0;
#pragma omp parallel for reduction(+:o)
  for (R_xlen_t i = 0; i < N; ++i) {
    o += xp[i];
  }
  if (o >= INT_MAX) {
    return ScalarReal(o);
  }
  return ScalarInteger(o);
}

