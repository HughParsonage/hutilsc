#include "hutilsc.h"

int radix_find(const int * k1p, const int a, int x0, int x1, int N) {
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
  
  if (k1p[m] < a) {
#if DEBUG > 1
    Rprintf("%d|> %d|  ", m, x1);
#endif
    return radix_find(k1p, a, m, x1, N);
  }
  
#if DEBUG > 1
  Rprintf("|%d <|%d  ", x0, m);
#endif
  return radix_find(k1p, a, x0, m, N);
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
  R_xlen_t R0 = radix_find(k1, x, 0, N, N);
  if (k1[R0] != x) {
    // not found
    // so never found
    R[0] = N - 1;
    R[1] = 0; // will use this to test
    return;
  }
  R_xlen_t R1 = R0; // largest index s.t. k1[R1] == x
  
  while ((R1 + 1) < N && k1[R1 + 1] == x) {
    ++R1;
  } 
  R[0] = R0;
  R[1] = R1;
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




