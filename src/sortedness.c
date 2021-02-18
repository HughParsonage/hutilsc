#include "hutilsc.h"

R_xlen_t which_isnt_sorted(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return 0;
  }
  R_xlen_t o = 0;
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i - 1] > xp[i]) {
        o = i; 
        break;
      }
    }
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i - 1] > xp[i]) {
        o = i; 
        break;
      }
    }
  }
    break;
  case STRSXP: {
    
    for (R_xlen_t i = 1; i < N; ++i) {
    const char * xpi0 = CHAR(STRING_ELT(x, i - 1));
    const char * xpi1 = CHAR(STRING_ELT(x, i));
    int s = strcmp(xpi0, xpi1);
      if (s > 0) {
        o = i; 
        break;
      }
    }
  }
    break;
    
    
  }
  return o;
}

bool sorted_int(const int * xp, R_xlen_t N, int nThreads) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= xp[i - 1] <= xp[i]; 
  }
  return (bool)o;
}

bool sorted_dbl(const double * xp, R_xlen_t N, int nThreads) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(& : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= xp[i - 1] <= xp[i]; 
  }
  return (bool)o;
}

bool sorted_str(SEXP x, R_xlen_t N, int nThreads) {
  char o = 1;
  for (R_xlen_t i = 1; i < N; ++i) {
    const char * xi0 = CHAR(STRING_ELT(x, i - 1));
    const char * xi1 = CHAR(STRING_ELT(x, i));
    int s = strcmp(xi0, xi1);
    o &= s <= 0;
  }
  return (bool)o;
}


SEXP do_which_isnt_sorted(SEXP x) {
  R_xlen_t o = which_isnt_sorted(x);
  return o <= INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

SEXP do_is_sorted(SEXP x, SEXP nThread) {
  int nThreads = asInteger(nThread);
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return_true;
  }
  
  char o = -1;
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: {
    const int * xp = INTEGER(x);
    o = sorted_int(xp, N, nThreads);
    return ScalarLogical(o);
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    o = sorted_dbl(xp, N, nThreads);
  }
    break;
  case STRSXP: {
    o = sorted_str(x, N, nThreads);
  }
    
    
  }
  if (o < 0) {
    return R_NilValue;
  }
  return ScalarLogical((bool)o);
}

SEXP do_unique_sorted(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t N128 = N > 128 ? 128 : N;
  R_xlen_t n_unique = 1, n_unique_128 = 1;
  if (TYPEOF(x) == NILSXP || N <= 1) {
    return x;
  }
  if (TYPEOF(x) == INTSXP) {
    const int * xp = INTEGER(x);
    
    int * mans = malloc(sizeof(int) * N);
    if (mans == NULL) {
      free(mans);
      return R_NilValue;
    }
    mans[0] = xp[0];
    R_xlen_t j = 1;
    for (R_xlen_t i = 1; i < N; ++i) {
      // i -- index of original
      // j -- index of new allocation
      bool new_element = xp[i - 1] != xp[i];
      n_unique += new_element;
      mans[j] = xp[i];
      j += new_element; // increment if new
    }
    SEXP ans = PROTECT(allocVector(INTSXP, j));
    int * restrict ansp = INTEGER(ans);
    for (R_xlen_t k = 0; k < j; ++k) {
      ansp[k] = mans[k];
    }
    free(mans);
    UNPROTECT(1);
    return ans;
  }
  return x;
}



int dig(int x, int d, int b) {
  const int digtens[9] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};
  int M = digtens[d];
  return (x % M) / (M / 10);
}

SEXP do_counting_sort(SEXP xo, SEXP base) {
  R_xlen_t N = xlength(xo);
  const int b = 10;
  
  const int * xop = INTEGER(xo);
  SEXP x = PROTECT(allocVector(INTSXP, N));
  int * xp = INTEGER(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xop[i] < 0 || xop[i] > 9) {
      xp[i] = 0;
    }
    xp[i] = xop[i];
  }
  
  int count[10];
  for (int c = 0; c < 10; ++c) {
    count[c] = 0;
  }
  
  unsigned int * xd = malloc(sizeof(int) * N);
  if (xd == NULL) {
    free(xd);
    error("xd not allocate.");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int xdi = dig(xp[i], 1, b);
    xd[i] = xdi;
    count[xdi] += 1;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xdi = xd[i];
    count[xdi] += 1;
  }
  count[1] += count[0];
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xdi = (unsigned int)xd[i];
    int pos = count[xdi] - 1;
    ansp[pos] = xp[i];
    --count[xdi];
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    xp[i] = ansp[i];
  }
  free(xd);
  UNPROTECT(2);
  return x;
}




