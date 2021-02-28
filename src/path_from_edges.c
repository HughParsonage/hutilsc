#include "hutilsc.h"

struct Node {
  int key_value;
  struct Node *left;
  struct Node *right;
};

inline int max0(int x) {
  return (x > 0) ? x : 0;
}

int Maxi(const int * xp, int N) {
  int o = 1;
  for (int i = 0; i < N; ++i) {
    int xpi = xp[i];
    o = xpi > o ? xpi : o;
  }
  return o;
}

// # nocov start
void print_vec(const int * xp, R_xlen_t N) {
  if (N < 20) {
    for (R_xlen_t i = 0; i < N; ++i) {
      Rprintf("%d,", xp[i]);
    }
  }
}
// # nocov end

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
    return ScalarInteger(INTSXP);
  }
  }
  return ScalarInteger(0);
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
    R_xlen_t R[2] = {0, -1};
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


SEXP do_common_contacts(SEXP aa, SEXP bb, SEXP K1, SEXP K2, SEXP Nodes, SEXP Len) {
  R_xlen_t N = xlength(K1);
  const int a = asInteger(aa);
  const int b = asInteger(bb);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  // const int len = asInteger(Len);
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

SEXP len3_paths(SEXP K1, SEXP K2, SEXP Nodes, SEXP return_nOutlets) {
  R_xlen_t N = xlength(K1);
  // # nocov start
  if (TYPEOF(K1) != INTSXP || 
      TYPEOF(K2) != INTSXP ||
      TYPEOF(Nodes) != INTSXP) {
    error("Internal error (len3_paths): input types not integer.");
  }
  // # nocov end
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  R_xlen_t UN = xlength(Nodes);
  const int * nodes = INTEGER(Nodes);
  if (!sorted_int(k1, N, 1)) {
    print_vec(k1, N);
    error("k1 is not sorted.");
  }
  if (!sorted_int(nodes, UN, 1)) {
    error("nodes is not sorted.");
  }
  const bool ret_nOutlets = asLogical(return_nOutlets);
  
  // # nocov start
  int * n_outlets = malloc(sizeof(int) * N);
  if (n_outlets == NULL) {
    free(n_outlets);
    error("Unable to allocate n_outlets.");
  }
  R_xlen_t * R0_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R0_outlets == NULL) {
    free(R0_outlets);
    error("Unable to allocate R0_outlets");
  }
  R_xlen_t * R1_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R1_outlets == NULL) {
    free(R1_outlets);
    error("Unable to allocate R1_outlets");
  }
  // # nocov end
  
  // Number of elements in result (number of len3 paths effectively)
  R_xlen_t AN = 0;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int k2i = k2[i];
    R_xlen_t R[2] = {-1, -1};
    radix_find_range(k2i, k1, R, N);
    R0_outlets[i] = R[0];
    R1_outlets[i] = R[1];
    int n_outletsi = R[1] - R[0];
    n_outletsi = n_outletsi < 0 ? 0 : (n_outletsi + 1);
    n_outlets[i] = n_outletsi;
    AN += n_outletsi;
  }
  
  if (ret_nOutlets) {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = n_outlets[i];
    }
    free(n_outlets);
    free(R0_outlets);
    free(R1_outlets);
    UNPROTECT(1);
    return ans;
  }
  
  if (AN < 1) {
    free(n_outlets);
    free(R0_outlets);
    free(R1_outlets);
    // no elements to speak of
    return R_NilValue;
  }
  
  SEXP ans1 = PROTECT(allocVector(INTSXP, AN));
  SEXP ans2 = PROTECT(allocVector(INTSXP, AN));
  SEXP ans3 = PROTECT(allocVector(INTSXP, AN));
  int * restrict ans1p = INTEGER(ans1);
  int * restrict ans2p = INTEGER(ans2);
  int * restrict ans3p = INTEGER(ans3);
  
  // k is the index of the output vectors (0 <= k < AN)
  R_xlen_t k = 0;
  
  // Loop through N elements, though the output vectors will be
  // length AN. Idea is to insert n_outlets[i] at each key
  // k1i, k2i.  If it's zero, no successor; otherwise, the number
  // of paths emanate from the node k2i along this edge. 
  // ans1p and ans2p are just the parent edge, though repeated
  // for convenience.
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int n_outletsi = n_outlets[i];
    if (n_outletsi) {
      R_xlen_t R0i = R0_outlets[i];
      for (int j = 0; j < n_outletsi; ++j, ++k) {
        ans1p[k] = k1[i];
        ans2p[k] = k2[i];
        ans3p[k] = k2[R0i + j];
      }
    }
  }
  free(n_outlets);
  free(R0_outlets);
  free(R1_outlets);
  
  SEXP ans = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ans, 0, ans1);
  SET_VECTOR_ELT(ans, 1, ans2);
  SET_VECTOR_ELT(ans, 2, ans3);
  UNPROTECT(4);
  return ans;
}


SEXP len4_paths(SEXP Len3Paths, SEXP K1, SEXP K2) {
  if (TYPEOF(Len3Paths) != VECSXP || xlength(Len3Paths) < 3) {
    error("Internal error(len4_paths): TYPEOF(Len3Paths) != VECSXP");
  }
  SEXP V0 = VECTOR_ELT(Len3Paths, 0);
  SEXP V1 = VECTOR_ELT(Len3Paths, 1);
  SEXP V2 = VECTOR_ELT(Len3Paths, 2);
  if (TYPEOF(V0) != INTSXP ||
      TYPEOF(V1) != INTSXP ||
      TYPEOF(V2) != INTSXP) {
    return R_NilValue;
  }
  
  R_xlen_t N = xlength(V0);
  if (N != xlength(V0) || N != xlength(V1) || N != xlength(V2)) {
    return R_NilValue;
  }
  
  R_xlen_t M = xlength(K1);
  if (M != xlength(K2)) {
    return R_NilValue;
  }
  
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  const int * v0 = INTEGER(V0);
  const int * v1 = INTEGER(V1);
  const int * v2 = INTEGER(V2);
  
  // # nocov start
  R_xlen_t * R0_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R0_outlets == NULL) {
    free(R0_outlets);
    error("Unable to allocate R0_outlets");
  }
  R_xlen_t * R1_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R1_outlets == NULL) {
    free(R1_outlets);
    error("Unable to allocate R1_outlets");
  }
  // # nocov end
  
  // Number of elements in result (number of len3 paths effectively)
  R_xlen_t AN = 0;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int k2i = v2[i];
    R_xlen_t R[2] = {-1, -1};
    radix_find_range(k2i, k1, R, M);
    R0_outlets[i] = R[0];
    R1_outlets[i] = R[1];
    R_xlen_t n_outletsi = R[1] - R[0];
    n_outletsi = n_outletsi < 0 ? 0 : (n_outletsi + 1);
    AN += n_outletsi;
  }
  if (AN > INT_MAX) {
    free(R0_outlets);
    free(R1_outlets);
    error("AN > INT_MAX in len4_paths");
  }
  
  R_xlen_t k = 0;
  
  SEXP ans0 = PROTECT(allocVector(INTSXP, AN));
  SEXP ans1 = PROTECT(allocVector(INTSXP, AN));
  SEXP ans2 = PROTECT(allocVector(INTSXP, AN));
  SEXP ans3 = PROTECT(allocVector(INTSXP, AN));
  int * restrict ans0p = INTEGER(ans0);
  int * restrict ans1p = INTEGER(ans1);
  int * restrict ans2p = INTEGER(ans2);
  int * restrict ans3p = INTEGER(ans3);
  for (R_xlen_t i = 0; i < N; ++i) {
    R_xlen_t R0 = R0_outlets[i];
    R_xlen_t R1 = R1_outlets[i];
    if (R1 >= R0) {
      R_xlen_t n_outletsi = R1 - R0 + 1;
      for (R_xlen_t j = 0; j < n_outletsi; ++j, ++k) {
        ans0p[k] = v0[i];
        ans1p[k] = v1[i];
        ans2p[k] = v2[i];
        ans3p[k] = k2[R0 + j];
      }
    }
  }
  free(R0_outlets);
  free(R1_outlets);
  SEXP ans = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  SET_VECTOR_ELT(ans, 2, ans2);
  SET_VECTOR_ELT(ans, 3, ans3);
  UNPROTECT(5);
  return ans;
}


void fuse2(const int * xp, const int * yp, int * zp, int N) {
  int M = Maxi(xp, N);
  int M1 = M + 1;
  // avoid malloc problems by asserting that M1 can never be -1
  if (M1 > 1e9 || M1 < 1) {
    return;
  }
  // original color --> new color map
  int * tbl = malloc(sizeof(int) * M1);
  if (tbl == NULL) {
    return;
  }
  for (int i = 0; i <= M; ++i) {
    tbl[i] = i;
  }
  //Rprintf("588\n");
  for (int i = N - 1; i >= 0; --i) {
    
    int ypi = yp[i];
    int xpi = xp[i];
    if (xpi != ypi) {
      int t = tbl[xpi];
      tbl[xpi] = ypi < t ? ypi : t;
    }
  }
  for (int j = 0; j < N; ++j) {
    //Rprintf(" j = %d,", j);
    int xpj = xp[j];
    //Rprintf("xpj = %d,\n", xpj);
    zp[j] = tbl[xpj];
  }
  free(tbl);
}

SEXP do_fuse2(SEXP x, SEXP y) {
  R_xlen_t N = xlength(x);
  if (xlength(y) != N || N > INT_MAX || TYPEOF(x) != INTSXP || TYPEOF(y) != INTSXP) {
    error("Lengths.");
  }
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  int * zp = malloc(sizeof(int) * N);
  if (zp == NULL) {
    free(zp);
    return R_NilValue;
  }
  fuse2(xp, yp, zp, (int)N);
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (int i = 0; i < N; ++i) {
    ansp[i] = zp[i];
  }
  free(zp);
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


SEXP do_validate_clique(SEXP K1, SEXP K2, SEXP Nodes, SEXP Clique) {
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(Nodes);
  if (N != xlength(K1) || N != xlength(K2) || N <= 1) {
    error("Lengths differ.");
  }
  const int * up = INTEGER(Nodes);
  const int * color = INTEGER(Clique);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  for (R_xlen_t i = 0; i < N; ++i) {
    int k1i = k1[i];
    int r1 = radix_find(up, k1i, 0, UN, UN);
    int c1 = color[r1];
    int k2i = k2[i];
    int r2 = radix_find(up, k2i, 0, UN, UN);
    int c2 = color[r2];
    if (c1 != c2) {
      Rprintf("%d,%d,%d | %d,%d ", k1i, r1, r2, c1, c2);
      return ScalarInteger(i + 1);
    }
  }
  return ScalarInteger(0);
}


void rev(const int * x, int * y, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    y[i] = x[(N - 1) - i];
  }
}

SEXP test_rev(SEXP x) {
  R_xlen_t N = xlength(x);
  int * r = malloc(sizeof(int) * N);
  const int * xp = INTEGER(x);
  rev(xp, r, N);
  SEXP out = PROTECT(allocVector(INTSXP, N));
  int * restrict outp = INTEGER(out);
  for (R_xlen_t i = 0; i < N; ++i) {
    outp[i] = r[i];
  }
  free(r);
  UNPROTECT(1);
  return out;
}

SEXP do_clique1(SEXP U, SEXP K1, SEXP K2, SEXP F1) {
  // Assumes a sequential
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(U);
  if (TYPEOF(U) != INTSXP || 
      TYPEOF(K1) != INTSXP ||
      TYPEOF(K2) != INTSXP) {
    error("Types integer.");
  }
  if (N != xlength(K2)) {
    error("N != xlength(K2)");
  }
  // const int * u = INTEGER(U);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  // const int * f1 = INTEGER(F1);
  
  // color each node
  SEXP ans = PROTECT(allocVector(INTSXP, UN));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < UN; ++i) {
    ansp[i] = 0;
  }
  int color = 1;
  // int new_color = 1;
  ansp[0] = 1;
  for (R_xlen_t i = 0; i < N; ++i) {
    int k1i = k1[i];
    int k2i = k2[i];
    int p1i = k1i - 1;
    int p2i = k2i - 1;
    
    if (ansp[p1i] == 0) {
      if (ansp[p2i] != 0) {
        ansp[p1i] = ansp[p2i];
      } else {
        ++color;
        ansp[p1i] = color;
        ansp[p2i] = color;
      }
    } else {
      color = ansp[p1i];
      ansp[p2i] = color;
    }
  }
  
  
  UNPROTECT(1);
  return ans;
}

SEXP do_fuse3(SEXP U, SEXP C, SEXP K1, SEXP K2) {
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(U);
  if (TYPEOF(U) != INTSXP || 
      TYPEOF(C) != INTSXP ||
      TYPEOF(K1) != INTSXP ||
      TYPEOF(K2) != INTSXP) {
    error("Types integer.");
  }
  if (N != xlength(K2) || UN != xlength(C)) {
    error("N != xlength(K2)");
  }
  // const int * u = INTEGER(U);
  const int * c = INTEGER(C);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  int min_c = 1; // always
  int max_c = 1;
  for (R_xlen_t i = 0; i < UN; ++i) {
    max_c = (c[i] < max_c) ? max_c : c[i];
  }
  int n_out = max_c - min_c + 1;
  SEXP ans = PROTECT(allocVector(INTSXP, n_out));
  int * restrict ansp = INTEGER(ans);
  
  // First assume that all colors are correctly entered
  for (R_xlen_t i = 0; i < n_out; ++i) {
    ansp[i] = i + 1;
  }
  
  // goal is to go through each color and 
  // if the colors are distinct at any edge
  // record the minimum color alongside the violating color
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int k1i = k1[i];
    int k2i = k2[i];
    
    // the color of a node ui is c[u[i] - 1]
    // (since u is sequential)
    int c1i = c[k1i - 1];
    int c2i = c[k2i - 1];
    if (c1i == c2i) {
      // all is well
      continue;
    }
    
    int min_colori = c1i < c2i ? c1i : c2i;
    int max_colori = c1i > c2i ? c1i : c2i;
    for (R_xlen_t ii = 0; ii < N; ++ii) {
      if (ansp[ii] == max_colori) {
        ansp[ii] = min_colori;
      }
    } 
  }
  UNPROTECT(1);
  return ans;
}

static bool venseq = false;

// given a sequence, i_1, i_2, i_3
// return the 1, 2, 3
SEXP do_enseq(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP || N == 0) {
    return R_NilValue;
  }
  int * xp = INTEGER(x);
  int xminmax[2] = {xp[0], xp[0]};
  Vminmax_i(xminmax, xp, N, 1);
  // the minimum is present
  if (xminmax[1] - xminmax[0] > INT_MAX) {
    warning("(do_enseq)Large range.");
    return R_NilValue;
  }
  
  // We need the first entry in the sequence to be 1
  if (xminmax[0] < 1) {
    warning("(do_enseq): xminmax[0] = %d < 1", xminmax[0]);
    return R_NilValue;
  }
  unsigned int n_range = xminmax[1] - xminmax[0] + 1;
  unsigned int dmin_from_1 = xminmax[0] - 1U;
  if (venseq) {
    Rprintf("n_range = %u\n", n_range);
    Rprintf("dmin_from_1 = %u\n", dmin_from_1);
  }
  
  // work out how many integers to subtract off
  // e.g. 1, 3, 4, 5, 7
  // want 1, 2, 3, 4, 5
  //      0, 1, 1, 1, 2
  // cumsum
  
  // First, detect the gaps (technically gaps[i] == 1 means 'no gap')
  unsigned char * gaps = calloc(n_range, sizeof(char));
  if (gaps == NULL) {
    warning("gaps could not be allocated");
    return R_NilValue;
  }
  
  // Our x is basically a flawed ans, so we allocate the result here to
  // allow ourselves to refer to ansp mostly
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = xp[i];
    ansp[i] = xi - dmin_from_1; // ensure it starts at 1;
    int pi = ansp[i] - 1;
    gaps[pi] |= 1; // opposite
  }
  
  
  unsigned int * necessary_cumsum = malloc(sizeof(int) * n_range);
  if (necessary_cumsum == NULL) {
    free(gaps);
    free(necessary_cumsum);
    UNPROTECT(1);
    return R_NilValue;
  }
  Rprintf("nc\n");
  necessary_cumsum[0] = 0U; // already established
  for (R_xlen_t i = 1; i < n_range; ++i) {
    necessary_cumsum[i] = necessary_cumsum[i - 1] + (1 - gaps[i]);
  }
  if (venseq) {
    for (R_xlen_t i = 0; i < n_range; ++i) {
      if (n_range < 100) {
        Rprintf("nc[%d] = %u,\n", i, necessary_cumsum[i]);
      }
    }
  }
  
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = ansp[i];
    if (venseq && (xi == NA_INTEGER || xi <= 0)) {
      Rprintf("i = %d was NA", i);
    }
    if (venseq && xi >= n_range + 1) {
      Rprintf("xi >= n_range at %d\n", i);
      continue;
    }
    int sub = necessary_cumsum[xi - 1];
    if (venseq && (sub > ansp[i] || sub < 0)) {
      Rprintf("sub = %d | ansp[i] = %d", sub, ansp[i]);
    }
    ansp[i] -= sub;
  }
  if (venseq) {
    Rprintf("loop complete\n");
  }
  free(necessary_cumsum);
  free(gaps);
  UNPROTECT(1);
  return ans;
}





