#include "hutilsc.h"


// radix find returns the first integer in k1p that is >= a, 
// [x0, x1) is the range that is searched
// N is the xlength of k1p
int pre_radix_find(const int * k1p, const int a, int x0, int x1, int N) {
  int w = x1 - x0;
#if DEBUG > 1
  Rprintf("\n%d;%d,%d ", w, x0, x1);
#endif
  if (w < 32) {
    for (int i = x0; i < x1; ++i) {
      if (k1p[i] >= a) {
        return i;
      }
    }
    return x1 - 1;
  }
  if (k1p[x0] >= a) {
    return x0;
  }
  if (k1p[x1 - 1] < a) {
    return x1 - 1;
  }
  
  int m = x0 + (w >> 1);
  if (k1p[m] == a) {
    return m;
  }
  if (k1p[m] < a) {
#if DEBUG > 1
    Rprintf("%d|> %d|  ", m, x1);
#endif
    return radix_find(k1p, a, m + 1, x1, N);
  }
  
#if DEBUG > 1
  Rprintf("|%d <|%d  ", x0, m);
#endif
  return radix_find(k1p, a, x0, m, N);
}

int radix_detect(int a, const int * k1p, int x0, int x1) {
  if (k1p[x0] == a) {
    return x0;
  }
  int w = x1 - x0;
  if (w < 16) {
    for (int i = x0; i < x1; ++i) {
      if (k1p[i] == a) {
        return i + 1;
      }
    }
    return 0;
  }
  int m = x0 + (w >> 1);
  if (k1p[m] < a) {
    return radix_detect(a, k1p, m, x1);
  } else {
    return radix_detect(a, k1p, x0, m + 1);
  }
}

int radix_find(const int * k1p, const int a, int x0, int x1, int N) {
  int o = pre_radix_find(k1p, a, x0, x1, N);
  while (o >= 1 && k1p[o - 1] == a) {
    --o;
  }
  
  return o;
}


SEXP do_test_radix_find(SEXP a, SEXP tbl, SEXP X0) {
  R_xlen_t N = xlength(tbl);
  const int aa = asInteger(a);
  int x0 = asInteger(X0);
  int x1 = (int)N;
  const int * kp = INTEGER(tbl);
  int r = radix_find(kp, aa, x0, x1, x1);
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = r;
  UNPROTECT(1);
  return ans;
}

void radix_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N) {
  // temporary linear search
  int R0 = 0;
  while (R0 < N && k1[R0] != x) {
    ++R0;
  }
  if (R0 == N) {
    R[0] = N;
    R[1] = 0;
  } else {
    R[0] = R0;
    while (R0 < N && k1[R0] == x) {
      R[1] = R0;
      ++R0;
    }
  }
}

SEXP do_test_radix_find_range(SEXP xx, SEXP K1) {
  const int x = asInteger(xx);
  const int * k1 = INTEGER(K1);
  const R_xlen_t N = xlength(K1);
  R_xlen_t R[2] = {0, 0};
  radix_find_range(x, k1, R, N);
  SEXP ans = PROTECT(allocVector(N <= INT_MAX ? INTSXP : REALSXP, 2));
  if (N <= INT_MAX) {
    INTEGER(ans)[0] = R[0];
    INTEGER(ans)[1] = R[1];
  } else {
    REAL(ans)[0] = R[0];
    REAL(ans)[1] = R[1];
  }
  UNPROTECT(1);
  return ans;
}

int linear_find(const int * k1, const int a, const int N) {
  int i = 0;
  while (i < N && k1[i] != a) {
    ++i;
  }
  return i;
}

int linear_find_from(const int * k1, const int a, int from, const int N) {
  int i = from;
  while (i < N && k1[i] != a) {
    ++i;
  }
  return i;
}
void linear_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N) {
  R_xlen_t R0 = 0;
  R_xlen_t R1 = N - 1;
  while (R0 < R1 && k1[R0] != x) {
    ++R0;
  }
  if (R0 == R1 && k1[R0] != x) {
    R[0] = -1;
    R[1] = 0;
    return;
  }
  while (R0 < R1 && k1[R1] != x) {
    --R1;
  }
  R[0] = R0;
  R[1] = R1;
}

SEXP n_sin(SEXP x, SEXP tbl, SEXP xsorted) {
  R_xlen_t M = xlength(tbl);
  R_xlen_t N = xlength(x);
  
  const int * xp = INTEGER(x);
  const int * tp = INTEGER(tbl);
  
  int n = 0;
  int xr = 0;
  
  R_xlen_t i = 0;
  int tpj = tp[0];
  while (i < N && xp[i] < tpj) {
    ++i;
  }
  
  for (; i < N; ++i) {
    
    int xi = xp[i];
    for (R_xlen_t j = xr; j < M; ++j) {
      tpj = tp[j];
      
      // need to keep incrementing for duplicated entires in x
      if (xi == tpj) {
        while (i < N && xp[i] == tpj) {
          ++n;
          ++i;
        }
        break;
      }
    }
  }
  return ScalarInteger(n);
}


SEXP do_find_ftc(SEXP x, SEXP tbl, SEXP nThreads, SEXP ret_lgl) {
  R_xlen_t N = xlength(x);
  R_xlen_t TN = xlength(tbl);
  if (TYPEOF(x) != INTSXP || TYPEOF(tbl) != INTSXP || TYPEOF(nThreads) != INTSXP) {
    return R_NilValue;
  }
  
  const int * tp = INTEGER(tbl);
  const int * xp = INTEGER(x);
  int nthread = asInteger(nThreads);
  const bool return_lgl = asLogical(ret_lgl);
  
  const int min_tb = tp[0];
  const int max_tb = tp[TN - 1];
  R_xlen_t n_full_table = max_tb - min_tb + 1;
  unsigned int n_full_table_ui = n_full_table;
  
  
  unsigned char * full_table = calloc(sizeof(char), n_full_table);
  if (full_table == NULL) {
    free(full_table);
    return R_NilValue;
  }
  
  for (R_xlen_t i = 0; i < TN; ++i) {
    int ti = tp[i];
    unsigned int tci = i;
    // Ensure it's never zero!
    full_table[ti - min_tb] = (unsigned char)((i & 255U) + 1U);
  }
  
  SEXP ans = PROTECT(allocVector(return_lgl ? LGLSXP : INTSXP, N));
  int * restrict ansp = INTEGER(ans);
#pragma omp parallel for num_threads(nthread)
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = xp[i];
    unsigned int pi = xi - min_tb;
    // pi >= n_full_table_ui means xi is out of range of table
    ansp[i] = (pi >= n_full_table_ui) ? 0 : full_table[pi];
  }
  free(full_table);
  UNPROTECT(1);
  return ans;
}


unsigned int find_first(int x, // value to search in k
                        const int * k,  // sorted array (with duplicates) 
                        R_xlen_t N, // length of k
                        int * kminmax,
                        const int * kp,  // given a value ki, kp[ki - mink] returns the position of ki in k
                        unsigned int NK, // number of elements of kp,
                        const int * up) {
  unsigned int mink = kminmax[0];
  unsigned int maxk = kminmax[0];
  unsigned int p = x - mink;
  if (p >= NK) {
    return 0U;
  }
  return kp[p] + 1U;
}

SEXP do_test_find_first(SEXP x, SEXP K1, SEXP U) {
  if (TYPEOF(x) != INTSXP || TYPEOF(K1) != INTSXP || TYPEOF(U) != INTSXP) {
    return R_NilValue;
  }
  if (xlength(K1) >= INT_MAX || xlength(U) >= INT_MAX) {
    return R_NilValue;
  }
  R_xlen_t n = xlength(x);
  int N = xlength(K1);
  int UN = xlength(U);
  
  const int * up = INTEGER(U);
  const int * k1 = INTEGER(K1);
  const int * xp = INTEGER(x);
  
  int uminmax[2] = {up[0], up[UN - 1]};
  unsigned int range_of_u = uminmax[1] - uminmax[0] + 1;
  
  // prepare table
  unsigned int * kp = calloc(range_of_u, sizeof(unsigned int));
  if (kp == NULL) {
    free(kp);
    return R_NilValue;
  }
  
  unsigned int jk1 = 0; // position in k1;
  for (int i = 0; i < UN; ++i) {
    // Loop through each element of U and find it in K
    int ui = up[i];
    
    // If we are below the current value of k1[j]
    // then we assigned that the value of 0
    if (ui < k1[jk1]) {
      kp[ui - uminmax[0]] = 0U;
      continue; // main loop will eventually hit k1
    }
    
    // If we match then we record the position
    if (ui == k1[jk1]) {
      kp[ui - uminmax[0]] = jk1 + 1U;
      
      // handle duplicates in k1 -- we only record the first
      while (jk1 < N && ui == k1[jk1]) {
        ++jk1;
      }
      continue;
    }
  }
  
  // So now kp[j] returns the position of j within k1;
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = xp[i];
    unsigned int xui = xi - uminmax[0];
    ansp[i] = kp[xui];
  }
  free(kp);
  UNPROTECT(1);
  return ans;
}







