#include "hutilsc.h"
#include <time.h>


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
      if (ansp[p2i] && ansp[p2i] != ansp[p1i]) {
        int min_color = (ansp[p2i] < ansp[p1i]) ? ansp[p2i] : ansp[p1i];
        int max_color = (ansp[p2i] > ansp[p1i]) ? ansp[p2i] : ansp[p1i];
        for (R_xlen_t j = 0; j < UN; ++j) {
          if (ansp[j] == max_color) {
            ansp[j] = min_color;
          }
        }
      } else {
        color = ansp[p1i];
        ansp[p2i] = color;
      }
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
SEXP Censeq(SEXP x, SEXP ZeroBased) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP || N == 0 || TYPEOF(ZeroBased) != LGLSXP) {
    return R_NilValue; // # nocov 
  }
  const int zero_based = asLogical(ZeroBased);
  
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
 
  int curr = zero_based ? 0 : 1;
  ansp[0] = curr;
  for (R_xlen_t i = 1; i < N; ++i) {
    curr += xp[i] != xp[i - 1];
    ansp[i] = curr;
  }
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

int n_paths_st(int d, int s, int t, int U0[], int U1[], const int k2[]) {
  // s >= t we mark as zero to avoid double counting
  // s -- t and t -- s
  // however, we need to double count k2[tc] -- t and t -- k2[tc]
  // in the loop below
  // since we may have s < t < v for v a linking node
  
  if (d == 0 || s >= t) {
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
    o += n_paths_st(d - 1, t, k2[tc], U0, U1, k2);
  }
  return o;
}

// weighted paths between s-t
unsigned int w_paths_st(int d, int s, int t, int U0[], int U1[], const int k2[], const int w[], unsigned int ans[256][256]) {
  if (d == 0) {
    return 0;
  }
  if (ans[s][t] < INT_MAX) {
    return ans[s][t];
  }
  if (ans[t][s] < INT_MAX) {
    ans[s][t] = ans[t][s];
    return ans[t][s];
  }
  int t_infra = U0[s - 1];
  int t_supra = U1[s - 1];
  if (t_supra < 0) {
    return 0;
  }
  unsigned int o = 0;
  if (d == 1) {
    for (int tc = t_infra; tc <= t_supra; ++tc) {
      o += w[tc] * (k2[tc] == t);
    }
    return o;
  }
  if (d == 2) {
    // s . t
    // number of paths is
    // number of paths from s -> .
    // times
    // number of paths from . -> t
    
    for (int tc = t_infra; tc <= t_supra; ++tc) {
      unsigned int ns_ = 0, n_t = 0;
      ns_ += w_paths_st(1, s, k2[tc], U0, U1, k2, w, ans);
      ns_ += w_paths_st(1, k2[tc], s, U0, U1, k2, w, ans);
      n_t += w_paths_st(1, k2[tc], t, U0, U1, k2, w, ans);
      n_t += w_paths_st(1, t, k2[tc], U0, U1, k2, w, ans);
      o += ns_ * n_t;
    }
    if (o) {
      ans[s][t] = o;
      ans[t][s] = o;
    }
    return o;
  }
  for (int tc = t_infra; tc <= t_supra; ++tc) {
    if (ans[k2[tc]][t] < INT_MAX) {
      o += ans[k2[tc]][t];
      continue;
    }
    o += w[tc] * w_paths_st(d - 1, k2[tc], t, U0, U1, k2, w, ans);
  }
  if (o) {
    ans[s][t] = o;
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



SEXP Cn_paths_svt0(SEXP ss, SEXP vv, SEXP tt,
                   SEXP K1, SEXP K2, SEXP W,
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
  const bool W_null = TYPEOF(W) == NILSXP;
  if (W_null) {
    if (notEquiInt2(K1, K2)) {
      error("notEquiInt2(K2, K2)");
    }
  } else {
    if (notEquiInt3(K1, K2, W)) {
      error("notEquiInt3(K2, K2, W)");
    } 
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
  if (xlength(U) >= 255) {
    error("xlength(U) >= 255.");
  }
  int nThread = as_nThread(nthreads);
  int UN = length(U);
  
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * ww = W_null ? INTEGER(K2) : INTEGER(W);
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
  
  unsigned int wpaths[256][256];
  unsigned char dists[256][256];
  for (int i = 0; i < 256; ++i) {
    for (int j = 0; j < 256; ++j) {
      wpaths[i][j] = INT_MAX;
      dists[i][j] = (i != j) * 255;
    }
  }
  
  if (xlength(ss) == 1) {
    int s = asInteger(ss);
    int t = asInteger(tt);
    if (s > t) {
      int tmp = s;
      s = t;
      t = tmp;
    }
    int dsvt = 0;
    // binary search for s in j1
    unsigned int Rs[2] = {1, 0};
    radix_find_range(s, j1, NULL, N, Rs);
    
    for (int j = Rs[0]; j <= Rs[1]; ++j) {
      if (dsvt) {
        break;
      }
      if (j1[j] == s) {
        for (int jj = j; (jj < N && j1[jj] == s); ++jj) {
          if (j2[jj] == t) {
            dsvt = d[jj];
            break;
          }
        }
      }
    }
    if (TYPEOF(vv) != INTSXP || xlength(vv) != 1) {
      int o = W_null ? n_paths_st(dsvt, s, t, U0, U1, k2) : w_paths_st(dsvt, s, t, U0, U1, k2, ww, wpaths);
      free(U0);
      free(U1);
      return ScalarInteger(o);
    }
    const int v = asInteger(vv);
    unsigned int osv = W_null ? n_paths_st(dsvt, s, v, U0, U1, k2) : w_paths_st(dsvt, s, v, U0, U1, k2, ww, wpaths);
    unsigned int ovt = W_null ? n_paths_st(dsvt, v, t, U0, U1, k2) : w_paths_st(dsvt, v, t, U0, U1, k2, ww, wpaths);
    unsigned int o = 0;
    if (W_null) {
      o = (int)(osv + ovt);
    } else {
      if (__builtin_umul_overflow(osv, ovt, &o)) {
        return(ScalarInteger(NA_INTEGER));
      }
    }
    free(U0);
    free(U1);
    return ScalarInteger(o);
  }
  
  const int * sp = INTEGER(ss);
  const int * tp = INTEGER(tt);
  
  R_xlen_t M = xlength(ss);
  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * restrict ansp = INTEGER(ans);
  int miss = 0;
  
  
  for (int di = 0; di < N; ++di) {
    int j1i = j1[di] - 1;
    int j2i = j2[di] - 1;
    dists[j1i][j2i] = (unsigned char)d[di];
  }
  
  if (TYPEOF(vv) == NILSXP) {
    // all paths
    for (R_xlen_t i = 0; i < M; ++i) {
      int s = sp[i];
      int t = tp[i];
      int di = dists[s - 1][t - 1];
      if (di > 8) {
        ansp[i] = NA_INTEGER;
        continue;
      }
      if (s > t) {
        int tmp = s;
        s = t;
        t = tmp;
      }
      ansp[i] = W_null ? n_paths_st(di, s, t, U0, U1, k2) : w_paths_st(di, s, t, U0, U1, k2, ww, wpaths);
    }
  } else {
    // n_paths s -- t including v
    
    if (xlength(vv) != M) {
      error("xlength(vv) != M"); 
    }
    
    const int * vp = INTEGER(vv);
    
    if (!W_null) {
      for (int i = 0; i < M; ++i) {
        ansp[i] = NA_INTEGER;
        int spi = sp[i];
        int vpi = sp[i];
        int tpi = vp[i];
        unsigned char d_si_vi = dists[spi][vpi];
        if (d_si_vi > 128) {
          continue;
        }
        unsigned char d_vi_ti = dists[vpi][tpi];
        if (d_vi_ti > 128) {
          continue;
        }
        unsigned int w_si_vi = w_paths_st(d_si_vi, spi, vpi, U0, U1, k2, ww, wpaths);
        unsigned int w_vi_ti = w_paths_st(d_vi_ti, vpi, tpi, U0, U1, k2, ww, wpaths);
        unsigned int anspi;
        if (__builtin_umul_overflow(w_si_vi, w_vi_ti, &anspi)) {
          continue;
        }
        ansp[i] = anspi;
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) 
#endif
      for (R_xlen_t i = 0; i < M; ++i) {
        ansp[i] = NA_INTEGER;
        int s = sp[i];
        int v = vp[i];
        int t = tp[i];
        unsigned char di = dists[s - 1][t - 1];
        if (di > 16) {
          continue;
        }
        unsigned int d_si_vi = dists[s - 1][v - 1];
        unsigned int d_vi_ti = dists[v - 1][t - 1];
        // if the total minimum distance from s -> v - t
        // is the greater than s -> t then there must be zero 
        // shortest paths through v
        if ((d_si_vi + d_vi_ti) > (unsigned int)di) {
          ansp[i] = 0;
          continue;
        }
        ansp[i] = n_paths_svt(di, s, v, t, U0, U1, k2);
      }
    }
  }
  free(U0);
  free(U1);
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



static const unsigned int MAX_BFS_DEPTH = 16u;


static void bfs(unsigned int depth, unsigned char ans[512][512], int orig0, int dest0, int U0[], int U1[], const int k1[], const int k2[], int N, unsigned int max_depth) {
  if (ans[orig0][dest0] != 255) {
    return;
  }
  
  // @param depth: current depth
  if (depth >= MAX_BFS_DEPTH) {
    ans[orig0][dest0] = 254;
    ans[dest0][orig0] = 254;
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
  }
  for (int kk = k1i; kk <= k2i; ++kk) {
    int k2kk = k2[kk] - 1;
    bfs(depth + 1u, ans, k2kk, dest0, U0, U1, k1, k2, N, max_depth);
    bfs(depth + 1u, ans, dest0, k2kk, U0, U1, k1, k2, N, max_depth);
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
  
  unsigned char bans[512][512];
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
  
  for (int dep = 0; dep <= 16; dep += 1) {
    for (int j = 0; j < N; ++j) {
      for (int i = 0; i < N; ++i) {
        if (i < j && bans[i][j] == 255) {
          bfs(1u, bans, i, j, U0, U1, k1, k2, N, dep);
        }
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
  // const double s = asReal(SS);
  
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
        // unsigned int wposij = wpos(i, j, N);
        // int wij = Wpos[wposij];
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



#define MAX_ID2 16384u

SEXP C_nFirstOrder(SEXP id1, SEXP id2, SEXP nid2, SEXP nthreads) {
  int err_equi3 = notEquiInt3(id1, id2, nid2);
  if (err_equi3) {
    error("notEquiInt3: [%d]", err_equi3);
  }
  
  
  
  int N = length(id1);
  int nThread = as_nThread(nthreads);
  int id2_minmax[2] = {INT_MAX, INT_MIN};
  const int * id1p = INTEGER(id1);
  const int * id2p = INTEGER(id2);
  //const int * nid2p = INTEGER(id2);
  if (__builtin_expect(!sorted_int(id1p, N, nThread), 0)) {
    error("id1p was not sorted."); // # nocov
  }
  
  Vminmax_i(id2_minmax, id2p, N, nThread);
  if (id2_minmax[1] > id2_minmax[0] + MAX_ID2) {
    return R_NilValue;
  }
  const int id2min = id2_minmax[0];
  const int id2max = id2_minmax[1];
  const int n_out = id2max - id2min;

  
  // Do this so they won't bound check within each loop
  // Check the first element
  int id1_0 = id1p[0];
  // Very unusual -- only one person in data
  if (id1p[N - 1] == id1_0) {
    SEXP ans = PROTECT(allocVector(INTSXP, n_out));
    for (int i = 0; i < n_out; ++i) {
      INTEGER(ans)[i] = 1;
    }
    UNPROTECT(1);
    return ans;
  }
  
  unsigned int o[MAX_ID2] = {0};
  
  // Count the number of people in each id2
  unsigned int n_staff_by_id2[MAX_ID2] = {0};
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : n_staff_by_id2[:MAX_ID2])
#endif 
  for (int i = 0; i < N; ++i) {
    n_staff_by_id2[id2p[i] - id2min] += 1;
  }
  
  SEXP SumById2 = PROTECT(allocVector(INTSXP, N));
  int * sum_by_id2 = INTEGER(SumById2);
  sum_by_id2[0] = n_staff_by_id2[id2p[0]];
  for (int i = 1; i < N; ++i) {
    int csum = sum_by_id2[i - 1];
    int n_staffi = n_staff_by_id2[id2p[i]];
    sum_by_id2[i] = n_staffi + (id1p[i - 1] == id1p[i]) * csum;
  }
  for (int i = N - 1; i >= 0; --i) {
    sum_by_id2[i] = (id1p[i + 1] == id1p[i]) ? sum_by_id2[i + 1] : sum_by_id2[i];
  }
  
  // Now we can evaluate directly by subtracting off the total sum by 
  for (int i = 0; i < N; ++i) {
    int id2i = id2p[i] - id2min;
    o[id2i] += n_staff_by_id2[id2i] - sum_by_id2[i];
  }
  SEXP ans = PROTECT(allocVector(INTSXP, n_out));
  int * restrict ansp = INTEGER(ans);
  for (int i = 0; i < n_out; ++i) {
    ansp[i] = o[i];
  }
  UNPROTECT(2);
  return ans;
}

int min4_n0(int a, int b, int c, int d) {
  if (!(a & b & c & d)) {
    return 0;
  }
  return 0;
}

unsigned char capAt254(unsigned char x, unsigned char y) {
  char o = x + y;
  return o < 0 ? 254 : o;
}


SEXP CDist2(SEXP K1, SEXP K2, SEXP UU) {
  if (notEquiInt2(K1, K2)) {
    error("notEquiInt2.");
  }
  if (TYPEOF(UU) != INTSXP) {
    error("UU not INTSXP.");
  }
  if (xlength(UU) > 250) {
    error("xlength(UU) > 250");
  }
  int kn = length(K1);
  int N = xlength(UU);
  const int * k1 = INTEGER(K1);
  const int * k2 = INTEGER(K2);
  const int * up = INTEGER(UU);
  
  unsigned short int dists[256][256];
  memset(dists, 0, sizeof(dists));
  
  
  for (int i = 0; i < kn; ++i) {
    int k1i = k1[i] - 1;
    int k2i = k2[i] - 1;
    dists[k1i][k2i] = 1;
    dists[k2i][k1i] = 1;
  }
  for (int i = 0; i < kn; ++i) {
    int k1i = k1[i] - 1;
    int k2i = k2[i] - 1;
    for (int j = 0; j < N; ++j) {
      if (!dists[j][k2i] && k2i != j && dists[j][k1i] == 1) {
        dists[j][k2i] = 2;
        dists[k2i][j] = 2;
      }
      if (!dists[j][k1i] && k1i != j && dists[j][k2i] == 1) {
        dists[j][k1i] = 2;
        dists[k1i][j] = 2;
      }
    }
  }
  
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
      if (i != j && dists[j][i]) {
        for (int k = 0; k < N; ++k) {
          if (i == k) {
            continue;
          }
          
          unsigned short int dist_ij = dists[k][j] + dists[j][i];
          // Select minimum distance, provided link between k and j already established
          if (dists[k][j] && (dists[k][i] == 0 || dists[k][i] >= dist_ij)) {
            dists[i][k] = dist_ij;
            dists[k][i] = dist_ij;
            
          }
        }
      }
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N * N));
  int * restrict ansp = INTEGER(ans);
  for (int k = 0, i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j, ++k) {
      ansp[k] = (i == j) ? 0 : dists[i][j];
    }
  }
  UNPROTECT(1);
  return ans;
}





