#include "hutilsc.h"

// input types
// SS = Same type
// ST = Different type
// 11 = both length one
// 1N = first len-1, 2 length N
#define INPUT2_SS11 1
#define INPUT2_SS1N 2
#define INPUT2_SSN1 3
#define INPUT2_SSNN 4
#define INPUT2_ST11 5
#define INPUT2_ST1N 6
#define INPUT2_STN1 7
#define INPUT2_STNN 8


static int input_types(SEXP X1, SEXP Y1, 
                       SEXP X2, SEXP Y2) {
  R_xlen_t N = xlength(X1);
  if (TYPEOF(X1) == TYPEOF(Y1) && 
      TYPEOF(X2) == TYPEOF(Y2)) {
    if (xlength(Y1) == 1 && xlength(Y2) == 1) {
      return INPUT2_SS11;
    }
    if (xlength(Y1) == 1 && xlength(Y2) == N) {
      return INPUT2_SS1N;
    }
    if (xlength(Y1) == N && xlength(Y2) == 1) {
      return INPUT2_SSN1;
    }
    if (xlength(Y1) == N && xlength(Y2) == N) {
      return INPUT2_SSNN;
    }
  } else {
    if (xlength(Y1) == 1 && xlength(Y2) == 1) {
      return INPUT2_ST11;
    }
    if (xlength(Y1) == 1 && xlength(Y2) == N) {
      return INPUT2_ST1N;
    }
    if (xlength(Y1) == N && xlength(Y2) == 1) {
      return INPUT2_STN1;
    }
    if (xlength(Y1) == N && xlength(Y2) == N) {
      return INPUT2_STNN;
    }
  }
  return 0;
}

static bool xiopyi(int x, int o, int y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}
static bool xiopyd(int x, int o, double y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}

// ii = x1 and x2 are integers
bool any_or2_ii(SEXP X1, SEXP O1, SEXP Y1,
                SEXP X2, SEXP O2, SEXP Y2, int nthreads) {
  if (TYPEOF(Y1) == REALSXP && TYPEOF(Y2) == INTSXP) {
    // don't duplicate code below just reverse order of expressions
    return any_or2_ii(X2, O2, Y2,
                      X1, O1, Y1, nthreads);
  }
  R_xlen_t N = xlength(X1);
  const int o1 = asInteger(O1);
  const int o2 = asInteger(O2);
  const int * x1p = INTEGER(X1);
  const int * x2p = INTEGER(X2);
  
  bool o = false;
  
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == 1 &&
      TYPEOF(Y2) == INTSXP && xlength(Y2) == 1) {
    const int y1 = asInteger(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyi(x2p[i], o2, y2)) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == 1 &&
      TYPEOF(Y2) == INTSXP && xlength(Y2) == N) {
    const int y1 = asInteger(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyi(x2p[i], o2, y2p[i])) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == N &&
      TYPEOF(Y2) == INTSXP && xlength(Y2) == 1) {
    const int * y1p = INTEGER(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyi(x2p[i], o2, y2)) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == N &&
      TYPEOF(Y2) == INTSXP && xlength(Y2) == N) {
    const int * y1p = INTEGER(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyi(x2p[i], o2, y2p[i])) {
        o = true;
        break;
      }
    }
    return o;
  }
  // real rhs
  
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == 1 &&
      TYPEOF(Y2) == REALSXP && xlength(Y2) == 1) {
    const int y1 = asInteger(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyd(x2p[i], o2, y2)) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == 1 &&
      TYPEOF(Y2) == REALSXP && xlength(Y2) == N) {
    const int y1 = asInteger(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyd(x2p[i], o2, y2p[i])) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == N &&
      TYPEOF(Y2) == REALSXP && xlength(Y2) == 1) {
    const int * y1p = INTEGER(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyd(x2p[i], o2, y2)) {
        o = true;
        break;
      }
    }
    return o;
  }
  if (TYPEOF(Y1) == INTSXP && xlength(Y1) == N &&
      TYPEOF(Y2) == REALSXP && xlength(Y2) == N) {
    const int * y1p = INTEGER(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyd(x2p[i], o2, y2p[i])) {
        o = true;
        break;
      }
    }
    return o;
  }
  return o;
}


SEXP do_any_or2(SEXP X1, SEXP O1, SEXP Y1,
              SEXP X2, SEXP O2, SEXP Y2,
              SEXP nThread) {
  R_xlen_t N = xlength(X1);
  if (N == 1) {
    return R_NilValue;
  }
  if (xlength(Y1) != N && xlength(Y1) != 1) {
    return R_NilValue;
  }
  if (xlength(X2) != N && xlength(X2) != 1) {
    return R_NilValue;
  }
  if (xlength(Y2) != N && xlength(Y2) != 1) {
    return R_NilValue;
  }
  if (TYPEOF(nThread) != INTSXP) {
    return R_NilValue;
  }
  int nthreads = asInteger(nThread);
  
  
  
  
  if (TYPEOF(X1) == INTSXP && TYPEOF(X2) == INTSXP) {
    if (TYPEOF(Y1) != INTSXP && TYPEOF(Y1) != REALSXP) {
      return R_NilValue;
    }
    if (TYPEOF(Y2) != INTSXP && TYPEOF(Y2) != REALSXP) {
      return R_NilValue;
    }
    bool o = false;
    o = any_or2_ii(X1, O1, Y1, X2, O2, Y2, nthreads);
    return ScalarLogical(o);
  }
  return R_NilValue;
}

SEXP Ccomplete_cases(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP) {
    error("Not integer.");
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int *restrict ansp = LOGICAL(ans);
  const int *xp = INTEGER(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = xp[i] != NA_INTEGER;
  }
  UNPROTECT(1);
  return ans;
}
