#include "hutilsc.h"

struct Node {
  int key_value;
  struct Node *left;
  struct Node *right;
};

inline int max0(int x) {
  return x < 0 ? 0 : x;
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

SEXP Censure_leq(SEXP K1, SEXP K2) {
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

bool one_valid_path(const int * pp, const int * k1, const int *k2, const int n, unsigned int N) {
  int a = pp[0];
  int r = radix_find(a, 0, N, k1, NULL);
  if (r < 0) {
    return false;
  }
  for (int i = 1; i < n; ++i) {
    unsigned int R[2] = {1U, 0U};
    radix_find_range(a, k1, NULL, N, R);
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

SEXP Cis_valid_path(SEXP path, SEXP K1, SEXP K2) {
  if (xlength(K1) >= UINT_MAX) {
    error("xlength(K1) >= UINT_MAX"); // # nocov
  }
  unsigned int N = xlength(K1);
  
  if (TYPEOF(path) != INTSXP || TYPEOF(K1) != INTSXP || TYPEOF(K2) != INTSXP ||
      xlength(K2) != N) {
    return R_NilValue; // # nocov
  }
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * pp = INTEGER(path);
  
  int n = xlength(path);
  
  bool o = one_valid_path(pp, k1, k2, n, N);
  return ScalarLogical(o);
}


SEXP Ccommon_contacts(SEXP aa, SEXP bb, SEXP K1, SEXP K2, SEXP Nodes, SEXP Len) {
  R_xlen_t N = xlength(K1);
  const int a = asInteger(aa);
  const int b = asInteger(bb);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  // const int len = asInteger(Len);
  R_xlen_t UN = xlength(Nodes);
  const int * nodes = INTEGER(Nodes);
  
  int r_a = radix_find(a, 0, UN, nodes, NULL);
  int r_b = radix_find(b, 0, UN, nodes, NULL);
  
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

SEXP len3_paths(SEXP K1, SEXP K2, SEXP Nodes) {
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
  // # nocov start
  if (!sorted_int(k1, N, 1)) {
    print_vec(k1, N);
    error("k1 is not sorted.");
  }
  if (!sorted_int(nodes, UN, 1)) {
    error("nodes is not sorted.");
  }
  
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
  
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  ftc2(U0, U1, k1, N);
  
  // Number of elements in result (number of len3 paths effectively)
  R_xlen_t AN = 0;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int k2i = k2[i];
    R_xlen_t R[2] = {-1, -1};
    R[0] = U0[k2i - 1];
    R[1] = U1[k2i - 1];
    R0_outlets[i] = R[0];
    R1_outlets[i] = R[1];
    int n_outletsi = R[1] - R[0];
    n_outletsi = n_outletsi < 0 ? 0 : (n_outletsi + 1);
    n_outlets[i] = n_outletsi;
    AN += n_outletsi;
  }
  
  if (AN < 1) {
    free(U0);
    free(U1);
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
  free(U0);
  free(U1);
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


SEXP len4_paths(SEXP Len3Paths, SEXP K1, SEXP K2, SEXP U) {
  if (TYPEOF(Len3Paths) != VECSXP || xlength(Len3Paths) < 3) {
    error("Internal error(len4_paths): TYPEOF(Len3Paths) != VECSXP"); // # nocov
  }
  SEXP V0 = VECTOR_ELT(Len3Paths, 0);
  SEXP V1 = VECTOR_ELT(Len3Paths, 1);
  SEXP V2 = VECTOR_ELT(Len3Paths, 2);
  // # nocov start
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
  // # nocov end
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  
  const int * v0 = INTEGER(V0);
  const int * v1 = INTEGER(V1);
  const int * v2 = INTEGER(V2);
  
  if (TYPEOF(U) != INTSXP) {
    error("TYPEOF(U) != INTSXP"); // # nocov
  }
  if (xlength(U) > INT_MAX) {
    error("xlength(U) > INT_MAX"); // # nocov
  }
  int UN = xlength(U);
  
  // # nocov start
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0);
    error("Unable to allocate U0.");
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0);
    free(U1);
    error("Unable to allocate U1");
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  R_xlen_t * R0_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R0_outlets == NULL) {
    free(U0);
    free(U1);
    free(R0_outlets);
    
    error("Unable to allocate R0_outlets");
  }
  R_xlen_t * R1_outlets = malloc(sizeof(R_xlen_t) * N);
  if (R1_outlets == NULL) {
    free(U0);
    free(U1);
    free(R0_outlets);
    free(R1_outlets);
    error("Unable to allocate R1_outlets"); 
  }
  // # nocov end
  
  ftc2(U0, U1, k1, M);
  
  
  
  // Number of elements in result (number of len3 paths effectively)
  R_xlen_t AN = 0;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int k2i = v2[i];
    R_xlen_t R[2] = {-1, -1};
    R[0] = U0[k2i - 1];
    R[1] = U1[k2i - 1];
    R0_outlets[i] = R[0];
    R1_outlets[i] = R[1];
    R_xlen_t n_outletsi = R[1] - R[0];
    n_outletsi = n_outletsi < 0 ? 0 : (n_outletsi + 1);
    AN += n_outletsi;
  }
  // # nocov start
  if (AN > INT_MAX) {
    free(U0);
    free(U1);
    free(R0_outlets);
    free(R1_outlets);
    error("AN > INT_MAX in len4_paths");
  }
  // # nocov end
  
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
  free(U0);
  free(U1);
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


SEXP Cvalidate_clique(SEXP K1, SEXP K2, SEXP Nodes, SEXP Clique) {
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(Nodes);
  if (N != xlength(K1) || N != xlength(K2) || N <= 1) {
    error("Lengths differ."); // # nocov
  }
  const int * up = INTEGER(Nodes);
  const int * color = INTEGER(Clique);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  for (R_xlen_t i = 0; i < N; ++i) {
    int k1i = k1[i];
    int r1 = radix_find(k1i, 0, UN, up, NULL) - 1;
    int c1 = color[r1];
    int k2i = k2[i];
    int r2 = radix_find(k2i, 0, UN, up, NULL) - 1;
    int c2 = color[r2];
    if (c1 != c2) {
      // Rprintf("%d,%d,%d | %d,%d ", k1i, r1, r2, c1, c2);
      return ScalarInteger(i + 1);
    }
  }
  return ScalarInteger(0);
}


SEXP Cclique1(SEXP U, SEXP K1, SEXP K2, SEXP F1) {
  // Assumes a sequential
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(U);
  if (TYPEOF(U) != INTSXP || 
      TYPEOF(K1) != INTSXP ||
      TYPEOF(K2) != INTSXP) {
    error("Types integer."); // # nocov
  }
  if (N != xlength(K2)) {
    error("N != xlength(K2)"); // # nocov
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

SEXP Cfuse3(SEXP U, SEXP C, SEXP K1, SEXP K2) {
  R_xlen_t N = xlength(K1);
  R_xlen_t UN = xlength(U);
  if (TYPEOF(U) != INTSXP || 
      TYPEOF(C) != INTSXP ||
      TYPEOF(K1) != INTSXP ||
      TYPEOF(K2) != INTSXP) {
    error("Types integer."); // # nocov
  }
  if (N != xlength(K2) || UN != xlength(C)) {
    error("N != xlength(K2)"); // # nocov
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
    for (R_xlen_t ii = 0; ii < n_out; ++ii) {
      if (ansp[ii] == max_colori) {
        ansp[ii] = min_colori;
      }
    } 
  }
  UNPROTECT(1);
  return ans;
}



// given a sequence, i_1, i_2, i_3
// return the 1, 2, 3
SEXP Censeq(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP || N == 0) {
    return R_NilValue; // # nocov 
  }
  
  const int * xp = INTEGER(x);
  int xminmax[2] = {xp[0], xp[0]};
  Vminmax_i(xminmax, xp, N, 1);
  // the minimum is present
  // # nocov start
  if (xminmax[1] - xminmax[0] > INT_MAX) {
    warning("(Censeq)Large range.");
    return R_NilValue;
  }
  
  // We need the first entry in the sequence to be 1
  if (xminmax[0] < 1) {
    warning("(Censeq): xminmax[0] = %d < 1", xminmax[0]);
    return R_NilValue;
  }
  unsigned int n_range = xminmax[1] - xminmax[0] + 1;
  unsigned int dmin_from_1 = xminmax[0] - 1U;
  // # nocov end
  
  // work out how many integers to subtract off
  // e.g. 1, 3, 4, 5, 7
  // want 1, 2, 3, 4, 5
  //      0, 1, 1, 1, 2
  // cumsum
  
  // First, detect the gaps (technically gaps[i] == 1 means 'no gap')
  unsigned char * gaps = calloc(n_range, sizeof(char));
  // # nocov start
  if (gaps == NULL) {
    warning("gaps could not be allocated");
    return R_NilValue;
  }
  // # nocov end
  
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
  // # nocov start
  if (necessary_cumsum == NULL) {
    free(gaps);
    free(necessary_cumsum);
    UNPROTECT(1);
    return R_NilValue;
  }
  // # nocov end
  
  necessary_cumsum[0] = 0U; // already established
  for (R_xlen_t i = 1; i < n_range; ++i) {
    necessary_cumsum[i] = necessary_cumsum[i - 1] + (1 - gaps[i]);
  }
  
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = ansp[i];
    int sub = necessary_cumsum[xi - 1];
    ansp[i] -= sub;
  }
  free(necessary_cumsum);
  free(gaps);
  UNPROTECT(1);
  return ans;
}


// Take a vertex, the order of that node,
// and a vector (ansp) that records the order from the original
// Return 
void next_neighb(const int max_order, 
                 int k2i_order, 
                 int k2i,
                 int * ansp,  // put the order here, will be initialized to NA_INTEGER
                 unsigned int * r_star, // locations of k2s
                 unsigned int * nr_star, // locations of k2s
                 R_xlen_t N,
                 const int * k1, 
                 const int * k2,
                 const int * nk1, 
                 const int * nk2) {
  unsigned int R[2] = {0, 0};
  radix_find_range(k2i, k1, r_star, N, R);
  for (unsigned int i = R[0]; i <= R[1]; ++i) {
    int k2ii = k2[i];
    if (ansp[k2ii - 1] == NA_INTEGER) {
      int this_order = k2i_order + 1;
      ansp[k2ii - 1] = this_order;
      if (this_order < max_order) {
        next_neighb(max_order,
                    this_order, 
                    k2ii,
                    ansp, 
                    r_star,
                    nr_star,
                    N, 
                    k1, 
                    k2,
                    nk1, 
                    nk2);
      }
    }
  }
  // Search reverse
  radix_find_range(k2i, nk1, nr_star, N, R);
  
  for (unsigned int i = R[0]; i <= R[1]; ++i) {
    int k2ii = nk2[i]; 
    if (ansp[k2ii - 1] == NA_INTEGER) {
      int this_order = k2i_order + 1;
      ansp[k2ii - 1] = this_order;
      if (this_order < max_order) {
        next_neighb(max_order,
                    this_order, 
                    k2ii,
                    ansp, 
                    r_star,
                    nr_star,
                    N, 
                    k1, 
                    k2,
                    nk1, 
                    nk2);
      }
    }
  }
}

SEXP Cego_net(SEXP vv,
              SEXP oo,
              SEXP K1, SEXP K2,
              SEXP NK1, SEXP NK2,
              SEXP nNodes) {
  const int v = asInteger(vv);
  const int o = asInteger(oo);
  R_xlen_t N = xlength(K1);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * nk1 = INTEGER(NK1);
  const int * nk2 = INTEGER(NK2);
  int M = asInteger(nNodes);
  if (M < 1 || M < v) {
    return R_NilValue; // # nocov
  }
  // # nocov start
  unsigned int * r_star = calloc(N + 2, sizeof(int));
  if (r_star == NULL) {
    free(r_star);
    return R_NilValue;
  }
  unsigned int * nr_star = calloc(N + 2, sizeof(int));
  if (nr_star == NULL) {
    free(r_star);
    free(nr_star);
    return R_NilValue;
  }
  // # nocov end
  
  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * ansp = INTEGER(ans); // don't use restrict
  
  // initialize with NAs
  for (R_xlen_t j = 0; j < M; ++j) {
    ansp[j] = NA_INTEGER;
  }
  
  // # nocov start
  if (v <= 0) {
    // v == 0 or NA means never there
    free(r_star);
    free(nr_star);
    UNPROTECT(1);
    return ans;
  }
  // # nocov end
  
  ansp[v - 1] = 0; // by definition, order=0 occurs at the node itself
  if (o == 0) {
    free(r_star);
    free(nr_star);
    UNPROTECT(1);
    return ans;
  }
  
  const int max_order = o > 10 ? 10 : o;
  unsigned int R[2] = {0, 0};
  radix_find_range(v, k1, r_star, N, R);
  //
  int this_order = 1; // discrete distance
  for (unsigned int i = R[0]; i <= R[1]; ++i) {
    int k2i = k2[i];
    ansp[k2i - 1] = 1;
    if (max_order >= 2) {
      next_neighb(max_order,
                  this_order, 
                  k2i,
                  ansp, 
                  r_star,
                  nr_star,
                  N, 
                  k1, 
                  k2,
                  nk1, 
                  nk2);
    }
  }
  radix_find_range(v, nk1, nr_star, N, R);
  for (unsigned int i = R[0]; i <= R[1]; ++i) {
    int k2i = nk2[i];
    if (ansp[k2i - 1] == NA_INTEGER) {
      ansp[k2i - 1] = 1;
      if (max_order >= 2) {
        next_neighb(max_order,
                    this_order, 
                    k2i,
                    ansp, 
                    r_star,
                    nr_star,
                    N, 
                    k1, 
                    k2,
                    nk1, 
                    nk2);
      }
    }
  }
  
  
  free(r_star);
  free(nr_star);
  UNPROTECT(1);
  return ans;
}

bool notInt(SEXP x) {
  return TYPEOF(x) != INTSXP || xlength(x) != 1;
}

bool notDbl(SEXP x) {
  return TYPEOF(x) != REALSXP || xlength(x) != 1;
}

int notEquiLgl2(SEXP x, SEXP y) {
  if (TYPEOF(x) != LGLSXP) {
    return 1;
  }
  if (TYPEOF(y) != LGLSXP) {
    return 2;
  }
  if (xlength(x) != xlength(y)) {
    return 3;
  }
  if (xlength(x) >= INT_MAX) {
    return 4;
  }
  return 0;
}

int notEquiInt2(SEXP x, SEXP y) {
  if (TYPEOF(x) != INTSXP) {
    return 1;
  }
  if (TYPEOF(y) != INTSXP) {
    return 2;
  }
  if (xlength(x) != xlength(y)) {
    return 3;
  }
  if (xlength(x) >= INT_MAX) {
    return 4;
  }
  return 0;
}

int notEquiInt3(SEXP x, SEXP y, SEXP z) {
  if (TYPEOF(x) != INTSXP) {
    return 1;
  }
  if (TYPEOF(y) != INTSXP) {
    return 2;
  }
  if (TYPEOF(z) != INTSXP) {
    return 3;
  }
  if (xlength(x) != xlength(y)) {
    return 4;
  }
  if (xlength(x) != xlength(z)) {
    return 5;
  }
  if (xlength(x) >= INT_MAX) {
    return 6;
  }
  return 0;
}

int notEquiReal2(SEXP x, SEXP y) {
  if (TYPEOF(x) != REALSXP) {
    return 1;
  }
  if (TYPEOF(y) != REALSXP) {
    return 2;
  }
  if (xlength(x) != xlength(y)) {
    return 3;
  }
  if (xlength(x) >= INT_MAX) {
    return 4;
  }
  return 0;
}

int n_paths_st(int d, int s, int t, int U0[], int U1[], const int k2[]) {
  if (d == 0) {
    return 0;
  }
  
  int t_infra = U0[s - 1];
  int t_supra = U1[s - 1];
  if (t_supra < 0) {
    return 0;
  }
  int o = 0;
  if (d == 1) {
    for (int tc = t_infra; tc <= t_supra; ++tc) {
      o += k2[tc] == t;
    }
    return o;
  }
  for (int tc = t_infra; tc <= t_supra; ++tc) {
    o += n_paths_st(d - 1, k2[tc], t, U0, U1, k2);
  }
  return o;
}


int n_paths_svt(int d, int s, int v, int t, 
                int U0[], int U1[], const int k2[]) {
  if (d <= 1) {
    return 0; // not possible to have three nodes on a path of len 1
  }
  
  // must be svt immediately
  int s_infra = U0[s - 1];
  int s_supra = U1[s - 1];
  if (s_supra < 0) {
    return 0;
  }
  int v_not_found = true;
  // vc = v candidate
  for (int vc = s_infra; vc <= s_supra; ++vc) {
    v_not_found &= k2[vc] != v;
  }
  if (d == 2) {
    if (v_not_found) {
      return 0;
    }
    int v_infra = U0[v - 1];
    
    int v_supra = U1[v - 1];
    if (v_supra < 0) {
      return 0;
    }
    for (int tc = v_infra; tc <= v_supra; ++tc) {
      if (k2[tc] == t) {
        return 1;
      }
    }
    return 0;
  }
  int o = 0;
  if (v_not_found) {
    for (int vc = s_infra; vc <= s_supra; ++vc) {
      o += n_paths_svt(d - 1, k2[vc], v, t, U0, U1, k2);
    }
  } else {
    
    for (int vc = s_infra; vc <= s_supra; ++vc) {
      if (k2[vc] == v) {
        o += n_paths_st(d - 1, v, t, U0, U1, k2);
      } else {
        o += n_paths_svt(d - 1, k2[vc], v, t, U0, U1, k2);
      }
    }
  }
  return o;
}


SEXP Cn_paths_svt0(SEXP ss, SEXP vv, SEXP tt, SEXP K1, SEXP K2,
                   SEXP U,
                   SEXP J1, SEXP J2, SEXP D,
                   SEXP nthreads) {
  if (TYPEOF(vv) != NILSXP) {
    if (notEquiInt3(ss, vv, tt)) {
      error("notEquiInt3(ss, vv, tt)");
    }
  } else {
    if (notEquiInt2(ss, tt)) {
      error("notEquiInt2(ss,, tt)");
    }
  }
  if (notEquiInt2(K1, K2)) {
    error("notEquiInt2(K2, K2)");
  }
  if (notEquiInt3(J1, J2, D)) {
    error("notEquiInt3(J1, J2, D) (no: %d)", notEquiInt3(J1, J2, D));
  }
  if (TYPEOF(ss) != INTSXP) {
    error("ss not integer.");
  }
  if (TYPEOF(tt) != INTSXP) {
    error("tt not integer.");
  }
  
  
  int nk = xlength(K1);
  if (xlength(U) >= INT_MAX) {
    error("xlength(U) >= INT_MAX");
  }
  int nThread = as_nThread(nthreads);
  int UN = length(U);
  
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  int N = length(J1);
  const int * j1 = INTEGER(J1);
  const int * j2 = INTEGER(J2);
  const int * d = INTEGER(D);
  
  for (int i = 0; i < nk; ++i) {
    unsigned int k1i = k1[i];
    if (k1i > UN) {
      error("At i = %d, k1[i] = %d whereas UN = %d\n", i, k1i, UN);
    }
  }
  for (int i = 0; i < N; ++i) {
    // j1 == UN is acceptable since it is 1-indexed
    unsigned int j1i = j1[i];
    if (j1i > UN) {
      error("At i = %d, j1[i] = %d whereas UN = %d\n", i, j1i, UN);
    }
  }
  
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  ftc2(U0, U1, k1, nk);
  if (xlength(ss) == 1) {
    const int s = asInteger(ss);
    const int t = asInteger(tt);
    
    int dsvt = 0;
    for (int j = 0; j < N; ++j) {
      if (dsvt) {
        break;
      }
      if (j1[j] == s) {
        for (int jj = j; (jj < N && j1[jj] == s); ++j) {
          if (j2[j] == t) {
            dsvt = d[jj];
            break;
          }
        }
      }
    }
    if (TYPEOF(vv) != INTSXP || xlength(vv) != 1) {
      int o = n_paths_st(dsvt, s, t, U0, U1, k2);
      free(U0);
      free(U1);
      return ScalarInteger(o);
    }
    const int v = asInteger(vv);
    int o = (n_paths_svt(dsvt, s, v, t, U0, U1, k2));
    free(U0);
    free(U1);
    return ScalarInteger(o);
  }
  
  R_xlen_t n_dist = xlength(D);
  int * S0 = malloc(sizeof(int) * n_dist);
  if (S0 == NULL) {
    free(U0);
    free(U1);
    free(S0);
    error("Unable to allocate S0");
  }
  int * S1 = malloc(sizeof(int) * n_dist);
  if (S1 == NULL) {
    free(U0);
    free(U1);
    free(S0);
    free(S1);
    error("Unable to allocate S1");
  }
  for (int i = 0; i < N; ++i) {
    S0[i] = 0;
    S1[i] = -1;
  }
  ftc2(S0, S1, j1, n_dist);
  
  R_xlen_t M = xlength(ss);
  if (xlength(vv) != M) {
    error("xlength(vv) != M"); 
  }
  
  const int * sp = INTEGER(ss);
  const int * vp = INTEGER(vv);
  const int * tp = INTEGER(tt);
  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * restrict ansp = INTEGER(ans);
  int miss = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : miss)
#endif
  for (R_xlen_t i = 0; i < M; ++i) {
    int s = sp[i];
    int v = vp[i];
    int t = tp[i];
    int di = 0;
    int sdi = S0[s - 1];
    int sds = S1[s - 1];
    if (sdi < 0) {
      if (miss == 0) {
        miss = i;
      }
      ansp[i] = NA_INTEGER;
      continue;
    }
    for (int id = sdi; id <= sds; ++id) {
      if (j2[id] == t) {
        di = d[id];
        break;
      }
    }
    ansp[i] = n_paths_svt(di, s, v, t, U0, U1, k2);
  }
  free(U0);
  free(U1);
  free(S0);
  free(S1);
  if (miss) {
    warning("Missing distance at position %d", (miss + 1));
  }
  UNPROTECT(1);
  return ans;
}

int n_paths_at_d(int d, int s_infra, int s_supra, int U0[], int U1[], const int k2[], int t) {
  if (d <= 1) {
    return 1;
  }
  int o = 0;
  if (d == 2) {
    for (int k = s_infra; k <= s_supra; ++k) {
      int v = k2[k];
      int v_infra = U0[v - 1];
      int v_supra = U1[v - 1];
      for (int kk = v_infra; kk <= v_supra; ++kk) {
        o += k2[kk] == t;
      }
    }
    return o;
  }
  for (int k = s_infra; k <= s_supra; ++k) {
    int v = k2[k];
    int v_infra = U0[v - 1];
    int v_supra = U1[v - 1];
    o += n_paths_at_d(d - 1, v_infra, v_supra, U0, U1, k2, t);
  }
  return o;
}

SEXP C_nPathsBetween_GivenDist(SEXP K1, SEXP K2, SEXP U, SEXP J1, SEXP J2, SEXP D) {
  //' @description Returns an integer vector of the number of paths between edges
  //' @param K1,K2 Integer vectors indicating edges
  //' @param J1,J2,D Integer vectors indicating distances between J1 and J2
  if (notEquiInt2(K1, K2)) {
    error("notEquiInt2(K2, K2)");
  }
  if (notEquiInt2(U, U)) {
    error("notEquiIn2(U, U)");
  }
  if (notEquiInt3(J1, J2, D)) {
    error("notEquiInt3(J1, J2, D)");
  }
  int UN = length(U);
  int nk = length(K1);
  int N = length(J1);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * j1 = INTEGER(J1);
  const int * j2 = INTEGER(J2);
  const int * d = INTEGER(D);
  
  for (int i = 0; i < nk; ++i) {
    unsigned int k1i = k1[i];
    if (k1i > UN) {
      error("At i = %d, k1[i] = %d whereas UN = %d\n", i, k1i, UN);
    }
  }
  for (int i = 0; i < N; ++i) {
    // j1 == UN is acceptable since it is 1-indexed
    unsigned int j1i = j1[i];
    if (j1i > UN) {
      error("At i = %d, j1[i] = %d whereas UN = %d\n", i, j1i, UN);
    }
  }
  
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  ftc2(U0, U1, k1, nk);
  
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * ansp = INTEGER(ans);
  
  for (int i = 0; i < N; ++i) {
    int di = d[i];
    if (di <= 1) {
      // typically this is distance = 1
      // so there is only one distance
      ansp[i] = di;
      continue;
    }
    int s = j1[i];
    int t = j2[i];
    
    // How many paths between s and t
    int s_infra = U0[s - 1];
    int s_supra = U1[s - 1];
    if (s_supra < 0) {
      ansp[i] = NA_INTEGER;
      continue; // very unlikely 
    }
    ansp[i] = 0;
    
    
    // distance = 2
    // 
    if (di == 2) {
      for (int k = s_infra; k <= s_supra; ++k) {
        int v = k2[k];
        int v_infra = U0[v - 1];
        int v_supra = U1[v - 1];
        for (int kk = v_infra; kk <= v_supra; ++kk) {
          ansp[i] += k2[kk] == t;
        }
      }
      continue;
    }
    if (di == 3) {
      for (int k = s_infra; k <= s_supra; ++k) {
        
        int v = k2[k];
        int v_infra = U0[v - 1];
        int v_supra = U1[v - 1];
        for (int kk = v_infra; kk <= v_supra; ++kk) {
          int vv = k2[kk];
          int vv_infra = U0[vv - 1];
          int vv_supra = U1[vv - 1];
          for (int kkk = vv_infra; kkk <= vv_supra; ++kkk) {
            ansp[i] += k2[kkk] == t;
          }
        }
      }
    } else {
      // int    n_paths_at_d(int d, int s_infra, int s_supra, int U0[], int U1[], int k2[], int t) {
      ansp[i] += n_paths_at_d(di, s_infra, s_supra, U0, U1, k2, t);
    }
  }
  free(U0);
  free(U1);
  UNPROTECT(1);
  return ans;
}


SEXP CBetweenessLen4(SEXP K1, SEXP K2, SEXP U, SEXP V1, SEXP V2, SEXP V3, SEXP V4) {
  if (TYPEOF(K1) != INTSXP ||
      TYPEOF(K1) != INTSXP ||
      TYPEOF(U)  != INTSXP ||
      TYPEOF(V1) != INTSXP ||
      TYPEOF(V2) != INTSXP ||
      TYPEOF(V3) != INTSXP ||
      TYPEOF(V4) != INTSXP) {
    error("Wrong type passed.");
  }
  if (xlength(K1) >= INT_MAX ||
      xlength(K1) >= INT_MAX ||
      xlength(U)  >= INT_MAX ||
      xlength(V1) >= INT_MAX ||
      xlength(V2) >= INT_MAX ||
      xlength(V3) >= INT_MAX ||
      xlength(V4) >= INT_MAX) {
    error("Lengths >= INT_MAX");
  }
  int N = length(K1);
  int UN = length(U);
  int N4 = length(V1);
  if (length(K1) != N ||
      length(K1) != N ||
      length(U)  != UN ||
      length(V1) != N4 ||
      length(V2) != N4 ||
      length(V3) != N4 ||
      length(V4) != N4) {
    error("Lengths mismatch");
  }
  const int * k1 = INTEGER(K1);
  // const int * k2 = INTEGER(K2);
  // const int * u = INTEGER(U);
  const int * v1 = INTEGER(V1);
  const int * v2 = INTEGER(V2);
  const int * v3 = INTEGER(V3);
  const int * v4 = INTEGER(V4);
  
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  ftc2(U0, U1, k1, N);
  
  // Calculate number of len4 paths from a to b immediately
  
  
  
  
  SEXP ans = PROTECT(allocVector(REALSXP, UN));
  double * ansp = REAL(ans);
  for (int v = 0; v < UN; ++v) {
    ansp[v] = 0;
  }
  
  for (int v = 0; v < UN; ++v) {
    for (int s = 0; s < v; ++s) {
      // int s_infra = U0[s];
      int s_supra = U1[s];
      if (s_supra < 0) {
        continue;
      }
      // position of s (if any in l4)
      int sv_infra = 0;
      while (sv_infra < N4 && v1[sv_infra] != s) {
        ++sv_infra;
      }
      // s not present in V1
      if (sv_infra >= N4) {
        continue;
      }
      int sv_supra = sv_infra;
      while (sv_supra < N4 && v1[sv_supra] == s) {
        ++sv_supra;
      }
      
      for (int t = v + 1; t < UN; ++t) {
        int n_len_4_paths_s_t = 0;
        int n_len_4_paths_svt = 0;
        for (int V4_i = sv_infra; V4_i < sv_supra; ++V4_i) {
          n_len_4_paths_s_t += v4[V4_i] == t;
          n_len_4_paths_svt += v2[V4_i] == v;
          n_len_4_paths_svt += v3[V4_i] == v;
        }
        if (n_len_4_paths_s_t) {
          double sigma_1 = n_len_4_paths_svt;
          double sigma_2 = n_len_4_paths_s_t;
          ansp[v] += sigma_1/sigma_2;
        }
      }
    }
  }
  UNPROTECT(1);
  return ans;
  
}

SEXP CBetweeness(SEXP K1, SEXP K2, SEXP U, SEXP J1, SEXP J2, SEXP D, SEXP maxPath) {
  //' @param K1,K2, equilength vectors indicating an edge exists between each K1[i]--K2[i]
  //' and K1[i] < K2[i]
  //' @param U Union of all nodes in order
  //' @param J1,J2,D equilength vectors indicating D[i] is the distance from J1[i] to J2[i];
  //' @param maxPath (int) the maximum distance a path that may be considered
  if (TYPEOF(K1) != INTSXP ||
      TYPEOF(K1) != INTSXP ||
      TYPEOF(U)  != INTSXP ||
      TYPEOF(J1) != INTSXP ||
      TYPEOF(J1) != INTSXP ||
      TYPEOF(D)  != INTSXP) {
    error("Wrong types passed to CBetweenenss"); // # nocov
  }
  R_xlen_t N = xlength(K1);
  if (xlength(K2) != N) {
    error("xlength(K2) != N"); // # nocov
  }
  if (xlength(U) >= INT_MAX) {
    error("xlength(U) >= INT_MAX"); // # nocov
  }
  if (xlength(maxPath) != 1 || 
      TYPEOF(maxPath) != INTSXP) {
    error("maxPath not an int."); // # nocov
  }
  if (xlength(J1) >= INT_MAX) {
    error("xlength(J1) >= INT_MAX");
  }
  int NJ = xlength(J1);
  if (NJ != xlength(J2) || 
      NJ != xlength(D)) {
    error("J1,J2,D mismatching lengths.");
  }
  
  int UN = length(U);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * u = INTEGER(U);
  const int * j1 = INTEGER(J1);
  const int * j2 = INTEGER(J2);
  const int * d = INTEGER(D);
  const int max_len = asInteger(maxPath);
  if (max_len < 2) {
    error("max_len < 2");
  }
  
  
  
  int * U0 = malloc(sizeof(int) * UN);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * UN);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  
  for (int i = 0; i < UN; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  
  ftc2(U0, U1, k1, N);
  
  
  SEXP ans = PROTECT(allocVector(REALSXP, UN));
  double * ansp = REAL(ans);
  // there must be at least three vertices
  // s < v < t
  //
  for (int j = 0; j < NJ; ++j) {
    int dj = d[j];
    if (dj >= 3 && dj <= max_len) {
      int s = j1[j];
      int t = j2[j];
      int us = u[s - 1];
      int us_infra = U0[us - 1];
      int us_supra = U1[us - 1];
      if (us_supra < 0) {
        continue;
      }
      
      switch(dj) {
      case 3: {
        // s v t
        for (int k = us_infra; k <= us_supra; ++k) {
        int v = k2[k];
        int v_infra = U0[v - 1];
        int v_supra = U1[v - 1];
        for (int jj = v_infra; jj < v_supra; ++jj) {
          ansp[v - 1] += k2[jj] == t;
        }
      }
      }
        break;
      case 4: {
        // s v _ t
        for (int kk = us_infra; kk <= us_supra; ++kk) {
        int k2kk = k2[kk];
        int k2kk_infra = U0[k2kk - 1];
        int k2kk_supra = U1[k2kk - 1];
        for (int kk3 = k2kk_infra; kk3 < k2kk_supra; ++kk3) {
          ansp[k2kk - 1] += k2[kk3] == t;
        }
      }
        
      } 
        
        // s _ v t
        
      }
    }
  }
  free(U0);
  free(U1);
  
  return R_NilValue;
}

static const unsigned int MAX_BFS_DEPTH = 64u;


static void bfs(unsigned int depth, unsigned char ans[1024][1024], int orig0, int dest0, int U0[], int U1[], const int k1[], const int k2[], int N) {
  // @param depth: current depth
  if (depth >= MAX_BFS_DEPTH) {
    ans[orig0][dest0] = 255;
    return;
  }
  if (dest0 == orig0) {
    ans[orig0][dest0] = 0;
    return;
  }
  if (ans[dest0][orig0] != 255) {
    ans[orig0][dest0] = ans[dest0][orig0];
    return;
  }
  
  // orig0,dest0 the origin and destination using ZERO INDEXING
  
  int k1i = U0[orig0];
  int k2i = U1[orig0];
  if (k2i < 0) {
    return;
  }
  for (int kk = k1i; kk <= k2i; ++kk) {
    int k2kk = k2[kk] - 1;
    
    if (k2kk == dest0) {
      ans[orig0][dest0] = depth;
      ans[dest0][orig0] = depth;
      break;
    }
    // if already known how far k2kk is from dest0 from existing searches
    unsigned char dist_k2kk_dest0 = ans[k2kk][dest0]; // assume == ans[dest0][k2kk]
    if (dist_k2kk_dest0 <= 250) {
      ans[orig0][dest0] = dist_k2kk_dest0 + 1;
      ans[dest0][orig0] = dist_k2kk_dest0 + 1;
      return;
    }
    bfs(depth + 1u, ans, k2kk, dest0, U0, U1, k1, k2, N);
    bfs(depth + 1u, ans, dest0, k2kk, U0, U1, k1, k2, N);
  }
}

SEXP Cdist_bw_edges(SEXP K1, SEXP K2, SEXP U) {
  if (notEquiInt2(K1, K2)) {
    error("notEquiInt2(K2, K2)");
  }
  int N = length(U);
  int kN = length(K1);
  if (N > 1023) {
    error("N > 1023, so N will exceed stack");
  }
  int N2 = N * N;
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  int * U0 = malloc(sizeof(int) * N);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * N);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  for (int i = 0; i < N; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  ftc2(U0, U1, k1, kN);
  
  unsigned char bans[1024][1024] = {255};
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
      bans[i][j] = (i == j) ? 0 : 255;
    }
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, N2));
  int * ansp = INTEGER(ans);
  for (int i = 0; i < N2; ++i) {
    ansp[i] = NA_INTEGER;
  }
  // 
  for (int i = 0; i < N; ++i) {
    int i_a = i * N; // index of ansp
    ansp[i_a + i] = 0;
    int k1i = U0[i];
    int k2i = U1[i];
    if (k2i > 0) {
      // all one distances
      for (int kk = k1i; kk <= k2i; ++kk) {
        int k2kk = k2[kk];
        
        unsigned int i_ab = i_a + (k2kk - 1);
        unsigned int i_ba = i + (k2kk - 1) * N;
        ansp[i_ab] = 1;
        ansp[i_ba] = 1;
        bans[i][k2kk - 1] = 1;
        bans[k2kk - 1][i] = 1;
        int kk1i = U0[k2kk - 1];
        int kk2i = U1[k2kk - 1];
        for (int kkk = kk1i; kkk <= kk2i; ++kkk) {
          int k2kkk = k2[kkk];
          unsigned int ii_ab = i_a + (k2kkk - 1);
          unsigned int ii_ba = i + (k2kkk - 1) * N;
          // assume all distances are symmetrically inserted
          if (ansp[ii_ab] == NA_INTEGER) {
            ansp[ii_ab] = 2;
            ansp[ii_ba] = 2;
            bans[i][k2kkk - 1] = 2;
            bans[k2kkk - 1][i] = 2;
          }
        }
      } 
    }
  }
  // int loops[1] = {1e5};
  
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
      if (i != j) {
        bfs(1u, bans, i, j, U0, U1, k1, k2, N);
      }
    } 
  }
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
      int ij = i * N + j;
      ansp[ij] = bans[i][j];
    }
  }
  
  free(U0);
  free(U1);
  UNPROTECT(1);
  return ans;
}

unsigned int utriangle(unsigned int N) {
  return (N % 2U) ? (N * ((N - 1U) / 2U)) : ((N / 2U) * (N - 1U));
}

unsigned int wpos(unsigned int i, unsigned int j, unsigned int N) {
  if (i <= j) {
    return -N + ((2U * N - 1U - i) * i) / 2U + j - 1U;
  }
  return wpos(j, i, N);
}

SEXP C_LayoutFruchtermanReingold1(SEXP UU, SEXP K1, SEXP K2, SEXP WW,
                                  SEXP GG, SEXP SS) {
  // GG,SS gravitational and spring constants
  if (notEquiInt3(K1, K2, WW)) {
    error("notEquiInt3(K1, K2, WW)"); // # nocov
  }
  if (!isInteger(UU)) {
    error("UU not an integer.");
  }
  if (TYPEOF(GG) != REALSXP || xlength(GG) != 1) {
    error("GG not num.");
  }
  if (TYPEOF(SS) != REALSXP || xlength(SS) != 1) {
    error("SS not num.");
  }
  R_xlen_t N = xlength(UU);
  R_xlen_t nk = xlength(K1);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * ww = INTEGER(WW);
  const double g = asReal(GG);
  const double s = asReal(SS);
  
  SEXP xx = PROTECT(allocVector(REALSXP, N));
  SEXP yy = PROTECT(allocVector(REALSXP, N));
  double * xp = REAL(xx);
  double * yp = REAL(yy);
  
  // Normalize
  SEXP xxminmax = Cminmax(xx, ScalarReal(0), ScalarInteger(1));
  SEXP yyminmax = Cminmax(yy, ScalarReal(0), ScalarInteger(1));
  double xminmax[2] = {REAL(xxminmax)[0], REAL(xxminmax)[1]};
  double yminmax[2] = {REAL(yyminmax)[0], REAL(yyminmax)[1]};
  double x_domain = xminmax[1] - xminmax[0];
  double y_domain = yminmax[1] - yminmax[0];
  if (x_domain > 2 && y_domain > 2) {
    for (R_xlen_t i = 0; i < N; ++i) {
      xp[i] /= x_domain;
      yp[i] /= y_domain;
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      xp[i] = (i + 0.5) / N;
      yp[i] = 0.5;
    }
  }
  
  xp[0] = 0;
  yp[0] = 0;
  
  int * U0 = malloc(sizeof(int) * N);
  if (U0 == NULL) {
    free(U0); // # nocov
    error("Unable to allocate U0."); // # nocov
  }
  int * U1 = malloc(sizeof(int) * N);
  if (U1 == NULL) {
    free(U0); // # nocov
    free(U1); // # nocov 
    error("Unable to allocate U1"); // # nocov
  }
  for (int i = 0; i < N; ++i) {
    U0[i] = 0;
    U1[i] = -1;
  }
  ftc2(U0, U1, k1, nk);
  
  
  // only consider i < j
  unsigned int TN = utriangle(N);
  int * Wpos = malloc(sizeof(int) * TN);
  if (Wpos == NULL) {
    free(U0);
    free(U1);
    free(Wpos);
    error("Unable to allocate Wpos"); // # nocov
  }
  for (unsigned int i = 0; i < TN; ++i) {
    Wpos[i] = 0;
  }
  for (int i = 0; i < nk; ++i) {
    int k1i = k1[i];
    int k2i = k2[i];
    int wi = ww[i];
    unsigned int wposi = wpos(k1i, k2i, N);
    if (wposi >= TN) {
      warning("k1i = %d, k2i = %d, wpois = %u >= %u = TN", k1i, k2i, wposi, TN); // # nocov
      wposi = 0;
    }
    Wpos[wposi] = wi;
  }
  
  
  
  for (int iter = 0; iter < 1024; ++iter) {
    if (!(iter & 15)) {
      R_CheckUserInterrupt();
      Rprintf("%f,%f\n", xp[0], yp[0]);
    }
    for (R_xlen_t i = 0; i < N; ++i) {
      double xpi = xp[i];
      double ypi = yp[i];
      for (R_xlen_t j = 0; j < N; ++j) {
        if (i == j) {
          continue;
        }
        unsigned int wposij = wpos(i, j, N);
        int wij = Wpos[wposij];
        double xpj = xp[j];
        double ypj = yp[j];
        double dx_ij = xpj - xpi;
        double dy_ij = ypj - ypi;
        if (dx_ij < 1 || dx_ij > -1) {
          dx_ij = 0;
        }
        if (dy_ij < 1 || dy_ij > -1) {
          dy_ij = 0;
        }
        double dist_ij_squared = dx_ij * dx_ij + dy_ij * dy_ij;
        // double dist_ij = ssqrt_fast((float)dist_ij_squared);
        if (dist_ij_squared > 0.5) {
          double F_repulsion = g / dist_ij_squared;
          xp[j] += F_repulsion * dx_ij;
          yp[j] += F_repulsion * dy_ij;
        }
      }
    }
  }
  
  
  free(U0);
  free(U1);
  free(Wpos);
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, xx);
  SET_VECTOR_ELT(ans, 1, yy);
  UNPROTECT(3);
  return ans;
}

SEXP qgraph_layout_Cpp(SEXP Pniter,
                       SEXP Pvcount,
                       SEXP Pecount,
                       SEXP Maxdelta,
                       SEXP Parea,
                       SEXP Pcoolexp,
                       SEXP Prepulserad,
                       SEXP EEf,
                       SEXP EEt, 
                       SEXP WW,
                       SEXP xxInit,
                       SEXP yyInit,
                       SEXP Cxx,
                       SEXP Cyy) {
  /*
   Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
   edgelist matrix d.  Positions (stored in (x,y)) should be initialized
   prior to calling this routine.
   */
  if (notInt(Pniter) || notInt(Pvcount) || notInt(Pecount)) {
    error("Expected int. (%d)", notInt(Pniter) + 2 * notInt(Pvcount) + 4 * notInt(Pecount)); // # nocov
  }
  int pniter = asInteger(Pniter);
  int pvcount = asInteger(Pvcount);
  int pecount = asInteger(Pecount);
  int n = pvcount;
  
  if (TYPEOF(Maxdelta) != REALSXP || xlength(Maxdelta) != n) {
    error("Maxdelta not REAL or xlength(Maxdelta) != n"); // # nocov
  }
  const double * maxdelta = REAL(Maxdelta);
  if (notDbl(Parea) || notDbl(Pcoolexp) || notDbl(Prepulserad)) {
    error("Expected dbl."); // # nocov
  }
  double parea = asReal(Parea);
  double pcoolexp = asReal(Pcoolexp);
  double prepulserad = asReal(Prepulserad);
  
  if (notEquiInt2(EEf, EEt)) {
    error("notEquiInt2(EEf, EEt)");
  }
  const int * Ef = INTEGER(EEf);   // Edges from 
  const int * Et = INTEGER(EEt);  // Edges t0
  
  const double * W = REAL(WW);
  if (TYPEOF(WW) != REALSXP || xlength(WW) != xlength(EEf)) {
    error("WW not REAL or equilength with EEf."); // # nocov
  }
  
  if (notEquiReal2(xxInit, yyInit)) {
    error("notEquiReal2(xxInit, yyInit)");
  }
  const double * xInit = REAL(xxInit);
  const double * yInit = REAL(yyInit);
  if (notEquiLgl2(Cxx, Cyy)) {
    error("notEquiLgl2(Cxx, Cyy)");
  }
  const int * Cx = LOGICAL(Cxx);
  const int * Cy = LOGICAL(Cyy);
  
  
  int m = pecount;
  double frk;
  double ded;
  double xd;
  double yd;
  double rf;
  double af;
  int i;
  int j;
  int k;
  int l;
  int niter = pniter;
  //double maxdelta;
  double area = parea;
  double coolexp = pcoolexp;
  double repulserad = prepulserad;
  
  // Unprotections
  int np = 0;
  SEXP dxx = PROTECT(allocVector(REALSXP, n)); np++;
  SEXP dyy = PROTECT(allocVector(REALSXP, n)); np++;
  SEXP tt = PROTECT(allocVector(REALSXP, n)); np++;
  double * dx = REAL(dxx);
  double * dy = REAL(dyy);
  double * t = REAL(tt);
  
  // Copy xIint and yInit:
  SEXP xx = PROTECT(allocVector(REALSXP, n)); np++;
  double * x = REAL(xx);
  SEXP yy = PROTECT(allocVector(REALSXP, n)); np++;
  double * y = REAL(yy);
  
  for (int i = 0; i < n; i++) {
    x[i] = xInit[i];
    y[i] = yInit[i];
  }
  
  frk = sqrt(area/(double)n);
  double frksq = area/((double)n);
  
  SEXP pows = PROTECT(allocVector(REALSXP, niter)); np++;
  double * powsp = REAL(pows);
  for (int i = 0; i < niter; ++i) {
    powsp[i] = pow((double)i / (double)niter, coolexp);
  }
  
  // Run the annealing loop
  for (i = niter; i >= 0; i--) {
    // Clear the deltas
    for (j = 0; j < n; j++){
      dx[j] = 0.0;
      dy[j] = 0.0;
    }
    // Increment deltas for each undirected pair
    for (j = 0; j < n; j++) {
      // Set the temperature (maximum move/iteration)
      t[j] = maxdelta[j] * powsp[i];
      
      for (k = j + 1; k < n; k++){
        // Obtain difference vector
        xd = x[j] - x[k];
        yd = y[j] - y[k];
        float dedsq = xd * xd + yd * yd;
        ded = ssqrt_fast(dedsq);  // Get dyadic euclidean distance
        xd /= ded;                // Rescale differences to length 1
        yd /= ded;
        // Calculate repulsive "force"
        rf = frksq * (1.0/ded - dedsq/repulserad);
        xd *= rf;
        yd *= rf;
        dx[j] += xd;        // Add to the position change vector
        dx[k] -= xd;
        dy[j] += yd;
        dy[k] -= yd;
      }
      
    }
    // Calculate the attractive "force"
    for (j = 0; j < m; j++) {
      k = Ef[j] - 1;
      l = Et[j] - 1;
      
      xd = x[k] - x[l];
      yd = y[k] - y[l];
      ded = euclid_dist_d(xd, yd);
      // ded = sqrt(xd*xd + yd*yd);  // Get dyadic euclidean distance
      if (ded > 0.000001 || ded < -0.000001) {
        xd /= ded;                // Rescale differences to length 1
        yd /= ded;
      }
      af = ded * ded / frk * W[j];
      dx[k] -= xd * af;        // Add to the position change vector
      dx[l] += xd * af;
      dy[k] -= yd * af;
      dy[l] += yd * af;
    }
    // Dampen motion, if needed, and move the points
    for (j = 0; j < n; j++) {
      // ded = sqrt(dx[j]*dx[j] + dy[j]*dy[j]);
      ded = euclid_dist_d(dx[j], dy[j]);
      if (ded > t[j]) {                 // Dampen to t
        ded = t[j] / ded;
        dx[j] *= ded;
        dy[j] *= ded;
      }
      if (!Cx[j]) {
        x[j] += round(dx[j] * 1e5) / 1e5; // Update positions (correcting for floating point errors)
      }
      if (!Cy[j]) {
        y[j] += round(dy[j] * 1e5) / 1e5;
      }
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2)); np++;
  SET_VECTOR_ELT(ans, 0, xx);
  SET_VECTOR_ELT(ans, 1, yy);
  UNPROTECT(np);
  return ans;
}



