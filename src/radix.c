#include "hutilsc.h"


//' pre_radix_find prepares radix find
//' @noRd
//' @description Find _a_ position of an integer in a sorted array.  In particular,
//' if `k1` contains duplicate values, there is no guarantee that the value
//' returned will be leftmost (which is often what is sought).
//' @param a Value to search for.
//' @param x0,x1  Defines the interval [x0, x1) in which the search should take place.
//' @param k1 Pointer to a sorted array in which a location of `a` is sought.
//' @param tbl A pointer to an elementary hash table.  
//' The value of tbl[a] returns (the position of a) + 1 in k1 or
//' 0 if the location is unknown 
//' UINT_MAX if a is absent.  (Idea is to eventually identify the leftmost
//' entry for a. Hence useful to have something that will always break loops
//' such as:
//' for (i = 0; i = UINT_MAX; i < N)
//' 
//' 
//' 
//' 

unsigned int pre_radix_find(int a, unsigned int x0, unsigned int x1, const int * k1) {
  // Try to get leftmost stuff as much as possible
  if (k1[x0] == a) {
    return x0 + 1;
  }
  unsigned int w = x1 - x0;
  if (w < 64) {
    for (unsigned int i = x0; i < x1; ++i) {
      if (k1[i] == a) {
        return i + 1;
      }
    }
    return UINT_MAX; // absent
  }
  unsigned int m = x0 + (w >> 1U);
  unsigned int k1m = k1[m];
  if (a == k1m) {
    return m + 1;
  } else if (a < k1m) {
    return pre_radix_find(a, x0, m + 1, k1);
  } else {
    return pre_radix_find(a, m, x1, k1);
  }
}

//' Find the location of the leftmost entry of a in k1
//' Crucially, `pre_radix_find()` is the 'true' radix find
//' but it stops whenever it finds a, not when it finds the first
//' 
unsigned int radix_find(int a, unsigned int x0, unsigned int x1, const int * k1, unsigned int * tbl) {
  if (tbl && tbl[a]) {
    return tbl[a];
  }
  unsigned int prf = pre_radix_find(a, x0, x1, k1);
  if (!prf || prf == UINT_MAX) {
    return prf;
  }
  // Do this so that the subsequent while loop doesn't require two conditions
  if (k1[0] == a) {
    if (tbl) {
      tbl[a] = 1U;
    }
    return 1U;
  }
  --prf; // prf is still 1-based
  while (k1[prf] == a) {
    --prf;
  }
  ++prf;
  if (tbl) {
    tbl[a] = prf + 1U;
  }
  return prf + 1U;
}

//' @noRd
//' @param a Value to search for.
//' @param k1 Pointer to a sorted array in which a location of `a` is sought.
//' @param tbl A pointer to an elementary hash table.  
//' The value of tbl[a] returns (the position of a) + 1 in k1 or
//' 0 if the location is unknown 
//' @param N The size of `k1`.
//' @param R Pointer to the output.
//' 
//' @return Called for its side-effect:
//' updates R so that
//' if `a` is not present in k1:
//' R[0] = UINT_MAX
//' R[1] = 0;
//' 
//' otherwise
//' R[0] = position (0-indexed) of first instance of `a` in k1
//' R[1] = position (0-indexed) of last instance.
//' 
//' N.B: the following loop will cover the exact interval
//' for (unsigned int i = R[0]; i <= R[1]; ++i)
//'                               ^^  
//'                               ^^  not <
//' 
//' 
//' 
void radix_find_range(int a, 
                      const int * k1,
                      unsigned int * tbl, 
                      unsigned int N,
                      unsigned int * R) {
  // radix_find is 1 based
  unsigned int R0 = radix_find(a, 0, N, k1, tbl);
  if (R0 == UINT_MAX) {
    R[0] = UINT_MAX;
    R[1] = 0U;
    return;
  }
  // Check the last value so that we don't have to check the bound
  // in the while loop
  if (k1[N - 1U] == a) {
    R[0] = R0 - 1U;
    R[1] = N - 1U;
    return;
  }
  unsigned int R1 = R0 - 1U;
  while (k1[R1] == a) {
    ++R1;
  }
  R[0] = R0 - 1U;
  R[1] = R1 - 1U;
}

SEXP Ctest_radix_find(SEXP a, SEXP tbl, SEXP X0) {
  R_xlen_t N = xlength(tbl);
  const int aa = asInteger(a);
  int x0 = asInteger(X0);
  int x1 = (int)N;
  const int * kp = INTEGER(tbl);
  int r = radix_find(aa, x0, x1, kp, NULL);
  return ScalarInteger(r);
}

SEXP Ctest_radix_find_range(SEXP xx, SEXP K1, SEXP usetp) {
  const bool use_tp = asLogical(usetp);
  const int * k1 = INTEGER(K1);
  const unsigned int N = xlength(K1);
  if (use_tp) {
    R_xlen_t Nx = xlength(xx);
    const int * xp = INTEGER(xx);
    const int min_k = k1[0];
    const int max_k = k1[N - 1];
    if (min_k < 0) {
      error("min_k < 0 (consider match on K1)."); // # nocov
    }
    
    SEXP ans = PROTECT(allocVector(INTSXP, 2 * Nx));
    int * restrict ansp = INTEGER(ans);
    unsigned int * tk = calloc((max_k + 2), sizeof(int));
    // # nocov start
    if (tk == NULL) {
      UNPROTECT(1);
      free(tk);
      error("Unable to allocate tk (try use_tp = FALSE).");
    }
    // # nocov end
    
    for (R_xlen_t i = 0; i < Nx; ++i) {
      int xi = xp[i];
      unsigned int R[2] = {0, 0};
      radix_find_range(xi, k1, tk, N, R);
      R_xlen_t j = 2 * i; // two entries for every found element
      ansp[j] = R[0];
      ansp[j + 1] = R[1];
    }
    free(tk);
    UNPROTECT(1);
    return ans;
  }
  const int x = asInteger(xx);
  unsigned int R[2] = {0, 0};
  
  radix_find_range(x, k1, NULL, N, R);
  SEXP ans = PROTECT(allocVector(N <= INT_MAX ? INTSXP : REALSXP, 2));
  if (N <= INT_MAX) {
    INTEGER(ans)[0] = R[0];
    INTEGER(ans)[1] = R[1];
  } else {
    REAL(ans)[0] = R[0]; // # nocov
    REAL(ans)[1] = R[1]; // # nocov
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cfind_ftc(SEXP x, SEXP tbl, SEXP nThreads, SEXP ret_lgl, SEXP ZeroBased) {
  R_xlen_t N = xlength(x);
  R_xlen_t TN = xlength(tbl);
  if (TYPEOF(x) != INTSXP || TYPEOF(tbl) != INTSXP || TYPEOF(nThreads) != INTSXP) {
    return R_NilValue; // # nocov
  }
  
  const int * tp = INTEGER(tbl);
  const int * xp = INTEGER(x);
  int nthread = asInteger(nThreads);
  const bool return_lgl = asLogical(ret_lgl);
  const bool zeroBased = asLogical(ZeroBased);
  
  const int min_tb = tp[0];
  const int max_tb = tp[TN - 1];
  R_xlen_t n_full_table = max_tb - min_tb + 1;
  unsigned int n_full_table_ui = n_full_table;
  
  
  unsigned char * full_table = calloc(sizeof(char), n_full_table);
  // # nocov start
  if (full_table == NULL) {
    free(full_table);
    return R_NilValue;
  }
  // # nocov end
  
  for (R_xlen_t i = 0; i < TN; ++i) {
    int ti = tp[i];
    unsigned int tci = i;
    // Ensure it's never zero!
    full_table[ti - min_tb] = (unsigned char)((tci & 255U) + 1U);
  }
  
  SEXP ans = PROTECT(allocVector(return_lgl ? LGLSXP : INTSXP, N));
  int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = xp[i];
    unsigned int pi = xi - min_tb;
    // pi >= n_full_table_ui means xi is out of range of table
    ansp[i] = (pi >= n_full_table_ui) ? 0 : full_table[pi];
  }
  free(full_table);
  if (zeroBased && !return_lgl) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] -= 1;
    }
  }

  UNPROTECT(1);
  return ans;
}

SEXP Ctest_find_first(SEXP x, SEXP K1, SEXP U) {
  // # nocov start
  if (TYPEOF(x) != INTSXP || TYPEOF(K1) != INTSXP || TYPEOF(U) != INTSXP) {
    return R_NilValue;
  }
  if (xlength(K1) >= INT_MAX || xlength(U) >= INT_MAX) {
    return R_NilValue;
  }
  // # nocov end
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
    free(kp); // # nocov
    return R_NilValue; // # nocov
  }
  
  unsigned int jk1 = 0; // position in k1;
  for (int i = 0; i < UN; ++i) {
    // Loop through each element of U and find it in K
    int ui = up[i];
    
    // If we are below the current value of k1[j]
    // then ui is not present in k1 so assign
    // zero to the lookup table
    
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
    if (xui >= range_of_u) { // also ensures xui > min
      ansp[i] = 0;
      continue;
    }
    ansp[i] = kp[xui];
  }
  free(kp);
  UNPROTECT(1);
  return ans;
}


// U0 -- indices of k1 such that k1[U0[i]] == i;
void ftc2(int * U0, int * U1, const int * k1, int N) {
  int i = 0;
  while (i < N) {
    int k1i = k1[i];
    U0[k1i - 1] = i;
    int j = i + 1;
    while (j < N && k1[j] == k1i) {
      ++j;
    }
    U1[k1i - 1] = j - 1;
    i = j;
  }
}


int cmpfunc(const void * a, const void * b) {
  return ( *(int*)a - *(int*)b );
}

int binary_find(int key, int * xp, int N) {
  int * res = (int *)bsearch(&key, xp, N, sizeof(int), cmpfunc);
  if (res) {
    int indx = res - &xp[0];
    while (indx >= 0 && xp[indx] == key) {
      --indx;
    }
    return indx + 1; 
  }
  return -1;
}

SEXP do_bsearch(SEXP a, SEXP x) {
  if (TYPEOF(a) != INTSXP || TYPEOF(x) != INTSXP) {
    return R_NilValue;
  }
  int * xp = INTEGER(x);
  
  R_xlen_t N = xlength(x);
  int key = asInteger(a);
  return ScalarInteger(binary_find(key, xp, N) + 1);
}





