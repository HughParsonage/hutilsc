#include "hutilsc.h"

struct Node {
  int key_value;
  struct Node *left;
  struct Node *right;
};

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

bool one_valid_path(const int * pp, const int * k1, const int *k2, const int n, R_xlen_t N) {
  int a = pp[0];
  int r = radix_find(k1, a, 0, N, N);
  if (k1[r] != a) {
    return false;
  }
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
      return false;
    }
    a = p2;
  }
  return true;
}

SEXP do_is_valid_path(SEXP path, SEXP K1, SEXP K2) {
  R_xlen_t N = xlength(K1);
  if (TYPEOF(path) != INTSXP) {
    return R_NilValue;
  }
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * pp = INTEGER(path);
  
  int n = xlength(path);
  
  bool o = one_valid_path(pp, k1, k2, n, N);
  return ScalarLogical(o);
}

SEXP do_reaches_between(SEXP aa, SEXP bb, SEXP K1, SEXP K2, SEXP Nodes) {
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
  warning("Not yet implemnted.");
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
  }
  
  UNPROTECT(1);
  return ans;
}


SEXP do_common_contacts(SEXP aa, SEXP bb, SEXP K1, SEXP K2, SEXP Nodes, SEXP Len) {
  R_xlen_t N = xlength(K1);
  const int a = asInteger(aa);
  const int b = asInteger(bb);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int len = asInteger(Len);
  
  
  R_xlen_t UN = xlength(Nodes);
  const int * nodes = INTEGER(Nodes);
  
  int r_a = radix_find(nodes, a, 0, UN, UN);
  int r_b = radix_find(nodes, b, 0, UN, UN);
  
  SEXP ans = PROTECT(allocVector(INTSXP, r_b - r_a));
  int * restrict ansp = INTEGER(ans);
  R_xlen_t n_common_cases = 0;
  
  
  for (R_xlen_t i = 0, j = r_a; j < r_b; ++j, ++i) {
    int pp[3] = {a, nodes[j], b};
    bool node_presentj = one_valid_path(pp, k1, k2, 3, N); 
    ansp[i] = nodes[j] * node_presentj;
    n_common_cases += node_presentj;
  }
  
  // The nodes properly indexed
  SEXP ans1 = PROTECT(allocVector(INTSXP, n_common_cases));
  int * restrict ans1p = INTEGER(ans1);
  for (R_xlen_t i = 0, k = 0, j = r_a; j < r_b; ++j, ++i) {
    ans1p[k] = ansp[i];
    k += (ansp[i] > 0);
  }
  
  UNPROTECT(2);
  return ans1;
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



