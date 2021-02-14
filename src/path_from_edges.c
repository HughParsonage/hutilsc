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
    // int sss = iisum(ansp, N);
    // Rprintf("sss = %d\n", sss);
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

SEXP do_ensure_leq(SEXP K1, SEXP K2) {
  if (TYPEOF(K1) != TYPEOF(K2)) {
    error("(ensure_leq): typeof differ.");
  }
  R_xlen_t N = xlength(K1);
  if (xlength(K2) != N) {
    error("(ensure_leq): xlengths differ.");
  }
  switch(TYPEOF(K1)) {
  case INTSXP: {
    int * k1 = INTEGER(K1);
    int * k2 = INTEGER(K2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (k1[i] > k2[i]) {
        int k1i = k1[i] + 0;
        int k2i = k2[i] + 0;
        k1[i] = k2i;
        k2[i] = k1i;
      }
    }
  }
  }
  return R_NilValue;
}

SEXP do_color_graph(SEXP K1, SEXP K2, SEXP Verb) {
  // color graph using the colors 1,2,3
  // where each color defines a separate group
  const R_xlen_t N = xlength(K1);
  bool verb = asLogical(Verb);
  
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
  if (verb) {
    Rprintf(".");
  }
  
  int color = 1;
  ansp[0] = 1;
  R_xlen_t i0 = 0;
  while (i0 < N && k1[i0] == k1[0]) {
    if (verb) {
      Rprintf("%d,", (int)i0);
    }
    ansp[i0] = 1;
    int k2i0 = k2[i0];
    R_xlen_t RR[2] = {-1, -1};
    radix_find_range(k2i0, k1, RR, N);
    for (R_xlen_t j = RR[0]; j <= RR[1]; ++j) {
      ansp[j] = 1;
    }
    ++i0;
  }
  if (verb) {
    Rprintf("\n N = %d\n", N);
  }
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (verb && ((i % 16) == 0) && i < INT_MAX) {
      Rprintf("i = %d,", i);
      Rprintf("ansp[i] = %d,", ansp[i]);
      Rprintf("color = %d\n", color);
    }
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
      if (verb && ((i % 16) == 0) && i < INT_MAX) {
        Rprintf("ii = %d\n", ii);
      }
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

SEXP touch_up_graph(SEXP Color, SEXP K1, SEXP K2, SEXP minColor) {
  R_xlen_t N = xlength(Color);
  if (N != xlength(K1) || N != xlength(K2) || N != xlength(minColor)) {
    error("Lengths differ.");
  }
  if (TYPEOF(Color) != INTSXP ||
      TYPEOF(K1) != INTSXP ||
      TYPEOF(K2) != INTSXP ||
      TYPEOF(minColor) != INTSXP) {
    error("Types not int.");
  }
  
  const int * color = INTEGER(Color);
  const int * mincolor = INTEGER(minColor);
  
  int * needs_changing = malloc(sizeof(int) * N);
  if (needs_changing == NULL) {
    return R_NilValue;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    needs_changing[i] = color[i] != mincolor[i];
  }
  
  int maxMinColor = mincolor[0];
  int maxColor = color[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    maxMinColor = (maxMinColor < mincolor[i]) ? mincolor[i] : maxMinColor;
    maxColor = (maxColor < color[i]) ? color[i] : maxColor;
  }
  int nColors = maxColor < maxMinColor ? maxMinColor : maxColor;
  
  int * old_color = malloc(sizeof(int) * nColors);
  if (old_color == NULL) {
    return R_NilValue;
  }
  int * new_color = malloc(sizeof(int) * nColors);
  if (new_color == NULL) {
    return R_NilValue;
  }
  for (int j = 0; j < nColors; ++j) {
    old_color[j] = j + 1;
    new_color[j] = j + 1;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int colori = color[i];
    ansp[i] = colori;
    if (color[i] != mincolor[i]) {
      int color_req_changing = color[i];
      int corrected_color = mincolor[i];
      new_color[color_req_changing - 1] = corrected_color;
    }
  }
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int colori = color[i];
    ansp[i] = needs_changing[i] ? new_color[colori - 1] : colori;
  }
  free(needs_changing);
  free(old_color);
  free(new_color);
  
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

SEXP do_is_valid_path(SEXP path, SEXP K1, SEXP K2) {
  R_xlen_t N = xlength(K1);
  if (TYPEOF(path) != INTSXP) {
    return R_NilValue;
  }
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * pp = INTEGER(path);
  
  int a = pp[0];
  int r = radix_find(k1, a, 0, N, N);
  if (k1[r] != a) {
    return_false;
  }
  int n = xlength(path);
  
  for (int i = 1; i < n; ++i) {
    
    R_xlen_t R[2] = {-1, -1};
    radix_find_range(a, k1, R, N);
    int p2 = pp[i];
    bool hits_next = false;
    for (R_xlen_t j = R[0]; j <= R[1]; ++j) {
      if (k2[j] == p2) {
        hits_next = true;
        break;
      }
    }
    if (!hits_next) {
      return_false;
    }
    a = p2;
  }
  return_true;
}

SEXP do_reaches_between(SEXP aa, SEXP bb, SEXP K1, SEXP K2) {
  const int a = asInteger(aa);
  const int b = asInteger(bb);
  
  if (b <= a + 1) {
    return R_NilValue;
  }
  
  R_xlen_t N = xlength(K1);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  R_xlen_t R[2] = {-1, -1};
  radix_find_range(a, k1, R, N);
  if (R[1] < R[0]) {
    return R_NilValue;
  }
  
  
  SEXP ans = PROTECT(allocVector(INTSXP, b));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < b; ++i) {
    ansp[i] = (i < a) ? 0 : NA_INTEGER;
  }
  
  for (int j = R[0]; j <= R[1]; ++j) {
    ansp[j] = 1;
    int destj = k2[j];
    if (destj < b) {
      ansp[destj] = 1;
    }
    
  }
  
  // Now go through each vertex from a to b,
  // Has it been reached? If so, it is reachable
  // And so are its descendants
  // If not, say so.
  for (int v = a; v < b; ++v) {
    if (ansp[v] == 1) {
      radix_find_range(v, k1, R, N);
      for (int j = R[0]; j <= R[1]; ++j) {
        ansp[j] = 1;
        int destj = k2[j];
        if (destj < b) {
          ansp[destj] = 1;
        }
      }
    } else {
      ansp[v] = 0;
    }
  }
  UNPROTECT(1);
  return ans;
}


SEXP do_fuse1(SEXP Color, SEXP K1, SEXP K2) {
  // fuse numbers
  R_xlen_t N = xlength(Color);
  if (N != xlength(K1) || N != xlength(K2) || N <= 1) {
    error("Lengths differ.");
  }
  const int * color = INTEGER(Color);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  int true_color = color[0];
  // start from 1 because the 1st entry is always true.
  for (R_xlen_t i = 1; i < N; ++i) {
    int colori = color[i];
    if (colori == true_color) {
      ansp[i] = colori;
      continue;
    }
    if (k2[i - 1] == k2[i] || k1[i - 1] == k1[i]) {
      // color is wrong and should be true color
      ansp[i] = true_color;
      continue;
    }
    ansp[i] = colori;
    true_color = colori;
    
  }
  UNPROTECT(1);
  return ans;
}



