#include "hutilsc.h"

struct Node {
  int key_value;
  struct Node *left;
  struct Node *right;
};

bool has_no_precedents(int k2i,
                       const int * k1, 
                       const int * k2,
                       const R_xlen_t N) {
  int r = radix_find(k1, k2i, 0, N, N);
  return k1[r] != k2i;
}

R_xlen_t apply_reaches_dest(int * ansp, 
                            const int * k1,
                            const int * k2,
                            R_xlen_t i, R_xlen_t N) {
  int k2i = k2[i];
  R_xlen_t r = radix_find(k1, k2i, 0, N, N);
  Rprintf("\t%d\n", (int) r);
  if (r < 0 || r >= N) {
    return 0;
  }
  if (k1[r] != k2i) {
    return 0;
  }
  while (k1[r] == k2i) {
    ansp[r] = 1;
    ++r;
  }
  return r;
}

int iisum(int * ap, R_xlen_t N) {
  int s = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    s += ap[i] > 0;
  }
  return s;
}

int unique_sorted_before(int x, const int * k, R_xlen_t N) {
  // k = a sorted array
  // x the value in k for which the number of unique values 
  // before is desired.
  if (x > k[N - 2]) {
    return N - 1;
  }
  if (x == k[0]) {
    return 0;
  }
  R_xlen_t i = 1;
  int o = 0;
  while (k[i] < x) {
    o += k[i] != k[i - 1];
    ++i;
  }
  return o;
}



SEXP reaches_dest(SEXP dest, SEXP K1, SEXP K2) {
  R_xlen_t N = xlength(K1);
  if (N != xlength(K2)) {
    error("Lengths.");
  }
  const int b = asInteger(dest);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  // RR is the range of values in kl which reach dest
  R_xlen_t RR[2] = {-1, 0};
  radix_find_range(b, k1, RR, N);
  R_xlen_t last_b = RR[0];
  while (last_b > 0 && k2[last_b] != b) {
    --last_b;
  }
  Rprintf("%d ", last_b);
  if (last_b == 0 && k2[last_b] != b) {
    return R_NilValue;
  }
  
  
  int k1_tmp = k1[last_b];
  radix_find_range(k1_tmp, k1, RR, N);
  
  Rprintf("k1_tmp = %d; RR0 = %d, RR1 = %d\n", k1_tmp, RR[0], RR[1]);
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
  }
  
  
  Rprintf("ansp0 = %d\nrr = ", ansp[0]);
  
  for (R_xlen_t rr = RR[0]; rr <= RR[1]; ++rr) {
    Rprintf("%d,", (int)rr);
    ansp[rr] = 1;
    int k1rr = k1[rr];
    for (R_xlen_t rri = 0; rri < RR[0]; ++rri) {
      if (k2[rri] > k1rr) {
        break;
      }
      if (k2[rri] == k1rr) {
        ansp[rri] = 1;
      }
    }
  }
  
  Rprintf(";\n ");
  
  int sum = iisum(ansp, N);
  R_xlen_t while_count = 0;
  
  R_xlen_t RR0 = RR[0];
  
  do {
    sum = iisum(ansp, N);
    Rprintf("sum = %d | ", sum);
    
    bool same_RR0 = true;
    // For every k1 entry currently,
    // find its location in k2, and then find 
    // the k1 location corresponding
    for (R_xlen_t i = 0; i < RR0; ++i) {
      if (ansp[i]) {
        Rprintf("%d: ", (int)i);
        if (same_RR0) {
          RR0 = i;
          same_RR0 = false;
        }
        int k1pi = k1[i];
        for (R_xlen_t j = 0; j < i; ++j) {
          int k2pj = k2[j];
          if (k1pi == k2pj) {
            ansp[j] = 1;
          }
        }
      }
    }
    int sss = iisum(ansp, N);
    Rprintf("sss = %d\n");
  } while (sum != iisum(ansp, N) && while_count++ < N);
    
  
  
  UNPROTECT(1);
  return ans;
}

int max_precendents(const int * k1, const R_xlen_t N) {
  // The maximum number of precedents is the maximum 
  // count in k1.  Since this is sorted, we only require
  // one simple pass through.
  
  int maxp = 0;
  int current_max = 1; // always at least one precedent
  for (R_xlen_t i = 1; i < N; ++i) {
    if (k1[i - 1] != k1[i]) { // new k1 value
      if (maxp > current_max) {
        maxp = current_max;
        current_max = 1;
      }
    }
  }
  return maxp;
}

SEXP color_graph(SEXP K1, SEXP K2) {
  // color graph using the colors 1,2,3
  // where each color defines a separate group
  const R_xlen_t N = xlength(K1);
  if (xlength(K2) != N) {
    error("Lengths differ.");
  }
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0; // INTEGER does not initialize
  }
  int color = 1;
  ansp[0] = 1;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ansp[i]) {
      R_xlen_t RR[2] = {-1, -1};
      radix_find_range(k2[i], k1, RR, N);
      for (R_xlen_t j = RR[0]; j <= RR[1]; ++j) {
        ansp[j] = ansp[i]; // color by existing coloring
      }
      
      continue; // already colored
    }
    // not reached by any previous node
    ++color;
    int k1i = k1[i];
    ansp[i] = color;
    R_xlen_t RR[2] = {-1, -1};
    radix_find_range(k2[i], k1, RR, N);
    for (R_xlen_t j = RR[0]; j <= RR[1]; ++j) {
      ansp[j] = color;
    }
    
    // Now do the same for the contiguous group
    for (R_xlen_t ii = i; (ii < N) && (k1[ii] == k1i); ++ii) {
      ansp[ii] = color;
      radix_find_range(k2[ii], k1, RR, N);
      for (R_xlen_t j = RR[0]; j <= RR[1]; ++j) {
        ansp[j] = color;
      }
    }
    
  }
  UNPROTECT(1);
  return ans;
}



SEXP do_path_from_edges(SEXP orig, SEXP dest, SEXP K1, SEXP K2) {
  int N = xlength(K1);
  const int a = asInteger(orig);
  const int b = asInteger(dest);
  
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  int d1 = radix_find(k1, a, 0, N, N);
  
  int * path = malloc(20 * sizeof(int));
  if (path == NULL) {
    error("Unable to allocate.");
  }
  
  int jj = d1;
  int k = 0;
  int destjj = k2[jj];
  for (; k < 20; ++k) {
    destjj = k2[jj];
    path[k] = destjj;
    if (destjj >= b) {
      break;
    }
    jj = linear_find_from(k1, destjj, jj, N);
  }
  if (destjj != b) {
    free(path);
    return R_NilValue;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, k));
  int * restrict ansp = INTEGER(ans);
  for (int i = 0; i < k; ++i) {
    ansp[i] = path[i];
  }
  free(path);
  UNPROTECT(1);
  return ans;
}
