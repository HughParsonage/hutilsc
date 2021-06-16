#include "hutilsc.h"






























// Just &
SEXP do_and_lgl_lgl(SEXP A, SEXP B, SEXP nThread) {
  R_xlen_t AN = xlength(A);
  R_xlen_t BN = xlength(B);
  if (AN == 0 || BN == 0) {
    SEXP ans = PROTECT(allocVector(LGLSXP, 0));
    UNPROTECT(1);
    return ans;
  }
  if (AN == 1 && BN == 1) {
    SEXP ans = PROTECT(allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = asLogical(A) && asLogical(B);
    UNPROTECT(1);
    return ans;
  }
  if (AN == 1) {
    const int a = asLogical(A);
    if (a == 0) {
      SEXP ans = PROTECT(allocVector(LGLSXP, BN));
      UNPROTECT(1);
      return ans;
    } else {
      return B;
    }
  }
  if (BN == 1) {
    const int b = asLogical(B);
    if (b == 0) {
      SEXP ans = PROTECT(allocVector(LGLSXP, AN));
      UNPROTECT(1);
      return ans;
    } else {
      return A;
    }
  }
  if (AN != BN) {
    error("Internal error(do_and_lgl_lgl): Lengths differ.");
  }
  
  int nThreads = asInteger(nThread);
  SEXP ans = PROTECT(allocVector(LGLSXP, BN));
  int *restrict ansp = INTEGER(ans);
  const int * ap = LOGICAL(A);
  const int * bp = LOGICAL(B);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif 
  for (R_xlen_t i = 0; i < AN; ++i) {
    ansp[i] = ap[i] && bp[i];
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cand_lgl_int(SEXP A, SEXP x2, SEXP op2, SEXP y2, SEXP nThread) {
  R_xlen_t AN = xlength(A);
  R_xlen_t xN = xlength(x2);
  R_xlen_t yN = xlength(y2);
  const int op = do_op2M(op2);
  
  if (AN == 1 && xN == 1 && yN == 1) {
    const int a = asLogical(A);
    if (a == 0) {
      return ScalarLogical(0);
    }
  }
  if (xN == 1 && yN == 1) {
    const int x1i = asInteger(x2);
    const int y1i = asInteger(y2);
    int o = 0;
    switch(op) {
    case OP_NE:
      o += x1i != y1i;
      break;
    case OP_IN:
    case OP_BW:
    case OP_BC:
    case OP_EQ:
      o += x1i == y1i;
      break;
    case OP_LT:
      o += x1i < y1i;
      break;
    case OP_LE:
      o += x1i <= y1i;
      break;
    case OP_GT:
      o += x1i > y1i;
      break;
    case OP_GE:
      o += x1i >= y1i;
      break;
    }
    if (o) {
      return A;
    } else if (AN == 1) {
      return ScalarLogical(0);
    } else {
      return allocVector(LGLSXP, AN);
    }
  }
  if (AN == xN && yN == 1) {
    int nThreads = asInteger(nThread);
    SEXP ans = PROTECT(allocVector(LGLSXP, xN));
    int * restrict ansp = LOGICAL(ans);
    const int * xp = INTEGER(x2);
    const int * Ap = LOGICAL(A);
    const int y = asInteger(y2);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif
    for (R_xlen_t i = 0; i < AN; ++i) {
      if (Ap[i] == 0) {
        ansp[i] = 0;
        continue;
      }
      if (Ap[i] == NA_LOGICAL) {
        ansp[i] = NA_LOGICAL;
        continue;
      }
      int o = 0;
      switch(op) {
      case OP_NE:
        o += xp[i] != y;
        break;
      case OP_IN:
      case OP_EQ:
        o += xp[i] == y;
        break;
      case OP_LT:
        o += xp[i] < y;
        break;
      case OP_LE:
        o += xp[i] <= y;
        break;
      case OP_GT:
        o += xp[i] > y;
        break;
      case OP_GE:
        o += xp[i] >= y;
        break;
      }
      ansp[i] = o;
    }
    UNPROTECT(1);
    return ans;      
  }
  return R_NilValue;
}

SEXP do_and_int_int(SEXP x1, SEXP op1, SEXP y1,
                    SEXP x2, SEXP op2, SEXP y2,
                    SEXP nThread) {
  const int o1 = do_op2M(op1);
  const int o2 = do_op2M(op2);
  if (o1 == OP_BW && xlength(y1) != 2) {
    return R_NilValue;
  }
  if (o2 == OP_BW && xlength(y2) != 2) {
    return R_NilValue;
  }
  
  if (o1 > o2) {
    // Avoid duplicating
    return(do_and_int_int(x2, op2, y2, 
                          x1, op1, y1,
                          nThread));
  }
  int nThreads = asInteger(nThread);
  R_xlen_t N = xlength(x1);
  
  if (N == 1) {
    return R_NilValue;
  }
  if (xlength(x2) != N) {
    return R_NilValue;
  }
  if (xlength(y1) != 1 && xlength(y1) != N) {
    return R_NilValue;
  }
  if (xlength(y2) != 1 && xlength(y2) != N && o2 != OP_IN && o2 != OP_BW) {
    return R_NilValue;
  }
  
  if (xlength(y1) == 1 && xlength(x2) == 1 && xlength(y2) == 1) {
    int Y = 1;
    const int x2i = asInteger(x2);
    const int y1i = asInteger(y1);
    const int y2i = asInteger(y2);
    switch(o2) {
    case OP_NE:
      Y = y1i != y2i; break;
    case OP_IN:
    case OP_EQ:
      Y = y1i == y2i; break;
    case OP_GE:
      Y = y1i > y2i; break;
    case OP_GT:
      Y = y1i >= y2i; break;
    case OP_LE:
      Y = y1i <= y2i; break;
    case OP_LT:
      Y = y1i < y2i; break;
    }
    if (Y == 0) {
      return allocVector(LGLSXP, N);
    }
    const int * x1p = INTEGER(x1);
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    int * restrict ansp = LOGICAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      int x1pi = x1p[i];
      int o = 0;
      switch(o1) {
      case OP_NE:
        o = x1p[i] != x2i; break;
      case OP_IN:
      case OP_EQ:
        o = x1pi == x2i; break;
      case OP_GE:
        o = x1pi > x2i; break;
      case OP_GT:
        o = x1pi >= x2i; break;
      case OP_LE:
        o = x1pi <= x2i; break;
      case OP_LT:
        o = x1pi < x2i; break;
      }
      ansp[i] = o ? TRUE : FALSE;
    }
    UNPROTECT(1);
    return ans;
  }
  
  const int * x1p = INTEGER(x1);
  const int * x2p = INTEGER(x2);
  const int * y1p = INTEGER(y1);
  const int * y2p = INTEGER(y2);
  
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int *restrict ansp = LOGICAL(ans);
  
  if (o1 == OP_IN && o2 == OP_IN) {
    R_xlen_t t1 = xlength(y1);
    R_xlen_t t2 = xlength(y2);
    
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
      int o = 0;
      for (R_xlen_t t = 0; t < t1; ++t) {
        if (x1p[i] == y1p[t]) {
          o += 1;
          break;
        }
      }
      if (o) {
        for (R_xlen_t t = 0; t < t2; ++t) {
          if (x2p[i] == y2p[t]) {
            ansp[i] = 1;
            break;
          }
        }
      }
    }
    UNPROTECT(1);
    return ans;
  }
  
  if (o1 == OP_BW && o2 == OP_BW) {
    if (xlength(y1) != 2 || xlength(y2) != 2) {
      error("Operation includes %between% but RHS was not length-2.");
    }
    const int a1 = INTEGER(y1)[0];
    const int b1 = INTEGER(y1)[1];
    const int a2 = INTEGER(y2)[0];
    const int b2 = INTEGER(y2)[1];
    if (a1 == NA_INTEGER && b1 == NA_INTEGER &&
        a2 == NA_INTEGER && b2 == NA_INTEGER) {
      // All TRUE
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = 1;
      }
      UNPROTECT(1);
      return ans;
    }
    if (a2 == NA_INTEGER && b2 == NA_INTEGER) {
      UNPROTECT(1);
      return do_and_int_int(x2, op2, y2, 
                            x1, op1, y1,
                            nThread);
    }
    
    if (a1 == NA_INTEGER && b1 == NA_INTEGER) {
      const int * x2p = INTEGER(x2);
      if (a2 == NA_INTEGER) {
        // just <=
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = x2p[i] <= b2;
        }
      } else if (b2 == NA_INTEGER) {
        // just >=
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = x2p[i] >= b1;
        }
      } else {
        for (R_xlen_t i = 0; i < N; ++i) {
          int x2pi = x2p[i];
          ansp[i] = x2pi >= a2 && x2pi <= b2;
        }
      }
      UNPROTECT(1);
      return ans;
    }
    
    // both ordinary between
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      int x1pi = x1p[i];
      int x2pi = x2p[i];
      
      ansp[i] = 
        x1pi >= a1 && x1pi <= b1 &&
        x2pi >= a2 && x2pi <= b2;
    }
    UNPROTECT(1);
    return ans; 
  }
  
  if (o1 == OP_IN && o2 == OP_BW) {
    const int y2_lwr = y2p[0];
    const int y2_upr = y2p[1];
    R_xlen_t M = xlength(y1);
    
    for (R_xlen_t i = 0; i < N; ++i) {
      char o = 0;
      ansp[i] = 0;
      int x1pi = x1p[i];
      for (R_xlen_t j = 0; j < M; ++j) {
        if (x1pi == y2p[j]) {
          o = 1;
          break;
        }
      }
      if (o) {
        ansp[i] = x2p[i] >= y2_lwr && x2p[i] <= y2_upr;
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (o2 == OP_IN) {
    R_xlen_t M = xlength(y2);
    if (xlength(y1) == 1) {
      const int y1pi = y1p[0];
      switch(o1) {
      case OP_NE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] != y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_EQ:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] == y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_GE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] >= y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_LE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] <= y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_GT:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] > y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_LT:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] < y1pi) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      }
    } 
    
    if (xlength(y1) == N) {
      switch(o1) {
      case OP_NE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] != y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_EQ:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] == y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_GE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] >= y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_LE:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] <= y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_GT:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] > y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      case OP_LT:
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = 0;
          if (x1p[i] < y1p[i]) {
            for (R_xlen_t j = 0; j < M; ++j) {
              if (x2p[i] == y2p[j]) {
                ansp[i] = 1;
                break;
              }
            }
          }
        }
        break;
      }
    }
    UNPROTECT(1);
    return ans;
  }
  
  if (xlength(x1) == xlength(x2) &&
      xlength(y1) == 1 && xlength(y2) == 1) {
    const int y_1 = asInteger(y1);
    const int y_2 = asInteger(y2);
    
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      int o = 1;
      int x1pi = x1p[i];
      int x2pi = x2p[i];
      switch(o1) {
      case OP_NE:
        switch(o2) {
        case OP_NE:
          o = (x1pi != y_1) && (x2pi != y_2); break;
        case OP_EQ:
          o = (x1pi != y_1) && (x2pi == y_2); break;
        case OP_GE:
          o = (x1pi != y_1) && (x2pi >= y_2); break;
        case OP_LE:
          o = (x1pi != y_1) && (x2pi <= y_2); break;
        case OP_GT:
          o = (x1pi != y_1) && (x2pi > y_2); break;
        case OP_LT:
          o = (x1pi != y_1) && (x2pi < y_2); break;
        }
        break;
      case OP_EQ:
        switch(o2) {
        case OP_EQ:
          o = (x1pi == y_1) && (x2pi == y_2); break;
        case OP_GE:
          o = (x1pi == y_1) && (x2pi >= y_2); break;
        case OP_LE:
          o = (x1pi == y_1) && (x2pi <= y_2); break;
        case OP_GT:
          o = (x1pi == y_1) && (x2pi > y_2); break;
        case OP_LT:
          o = (x1pi == y_1) && (x2pi < y_2); break;
        }
        break;
      case OP_GE:
        switch(o2) {
        case OP_GE:
          o = (x1pi >= y_1) && (x2pi >= y_2); break;
        case OP_LE:
          o = (x1pi >= y_1) && (x2pi <= y_2); break;
        case OP_GT:
          o = (x1pi >= y_1) && (x2pi > y_2); break;
        case OP_LT:
          o = (x1pi >= y_1) && (x2pi < y_2); break;
        }
        break;
      case OP_LE:
        switch(o2) {
        case OP_LE:
          o = (x1pi <= y_1) && (x2pi <= y_2); break;
        case OP_GT:
          o = (x1pi <= y_1) && (x2pi > y_2); break;
        case OP_LT:
          o = (x1pi <= y_1) && (x2pi < y_2); break;
        }
        break;
      case OP_GT:
        switch(o2) {
        case OP_GT:
          o = (x1pi > y_1) && (x2pi > y_2); break;
        case OP_LT:
          o = (x1pi > y_1) && (x2pi < y_2); break;
        }
        break;
      case OP_LT:
        o = (x1pi < y_1) && (x2pi < y_2); break;
      }
      ansp[i] = o;
    }
    UNPROTECT(1);
    return ans;
  }
  
  if (xlength(x1) == xlength(y1) && 
      xlength(x2) == xlength(y2)) {
    const int * x1p = INTEGER(x1);
    const int * y1p = INTEGER(y1);
    
    const int * x2p = INTEGER(x2);
    const int * y2p = INTEGER(y2);
    
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      int o = 1;
      int x1pi = x1p[i];
      int y1pi = y1p[i];

      
      switch(o1) {
      case OP_NE:
        o = x1pi != y1pi; break;
      case OP_EQ:
        o = x1pi == y1pi; break;
      case OP_GE:
        o = x1pi >= y1pi; break;
      case OP_GT:
        o = x1pi >  y1pi; break;
      case OP_LE:
        o = x1pi <= y1pi; break;
      case OP_LT:
        o = x1pi <  y1pi; break;
      }
      if (o) {
        int x2pi = x2p[i];
        int y2pi = y2p[i];
        switch(o2) {
        case OP_NE:
          o = x2pi != y2pi; break;
        case OP_EQ:
          o = x2pi == y2pi; break;
        case OP_GE:
          o = x2pi >= y2pi; break;
        case OP_GT:
          o = x2pi >  y2pi; break;
        case OP_LE:
          o = x2pi <= y2pi; break;
        case OP_LT:
          o = x2pi <  y2pi; break;
        }
      }
      ansp[i] = o ? 1 : 0;
    }
    UNPROTECT(1);
    return ans;
  }
  UNPROTECT(1);
  
  return R_NilValue;
}

int scalar_xoy(SEXP x, int op, SEXP y) {
  switch(op) {
  case OP_NE:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 != y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 != y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 != y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 != y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 != y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 != y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 != y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 != y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 != y0;
      }
        break;
        return NA_INTEGER;
      }
    }
    break;
  case OP_IN:
  case OP_EQ:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 == y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 == y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 == y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 == y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 == y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 == y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 == y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 == y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 == y0;
      }
        break;
        return NA_INTEGER;
      }
      
    }
    break;
  case OP_GE:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 >= y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 >= y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 >= y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 >= y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 >= y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 >= y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 >= y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 >= y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 >= y0;
      }
        break;
        return NA_INTEGER;
      }
    }
    break;
  case OP_LE:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 <= y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 <= y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 <= y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 <= y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 <= y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 <= y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 <= y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 <= y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 <= y0;
      }
        break;
        return NA_INTEGER;
      }
    }
    break;
  case OP_GT:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 > y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 > y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 > y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 > y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 > y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 > y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 > y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 > y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 > y0;
      }
        break;
        return NA_INTEGER;
      }
    }
    break;
  case OP_LT:
    switch(TYPEOF(x)) {
    case LGLSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asLogical(x);
        const int y0 = asLogical(y);
        return x0 < y0;
      }
        break;
      case INTSXP: {
        const int x0 = asLogical(x);
        const int y0 = asInteger(y);
        return x0 < y0;
      }
        break;
      case REALSXP: {
        const int x0 = asLogical(x);
        const double y0 = asReal(y);
        return x0 < y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case INTSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const int x0 = asInteger(x);
        const int y0 = asLogical(y);
        return x0 < y0;
      }
        break;
      case INTSXP: {
        const int x0 = asInteger(x);
        const int y0 = asInteger(y);
        return x0 < y0;
      }
        break;
      case REALSXP: {
        const int x0 = asInteger(x);
        const double y0 = asReal(y);
        return x0 < y0;
      }
        break;
        return NA_INTEGER;
      }
      break;
    case REALSXP:
      switch(TYPEOF(y)) {
      case LGLSXP: {
        const double x0 = asReal(x);
        const int y0 = asLogical(y);
        return x0 < y0;
      }
        break;
      case INTSXP: {
        const double x0 = asReal(x);
        const int y0 = asInteger(y);
        return x0 < y0;
      }
        break;
      case REALSXP: {
        const double x0 = asReal(x);
        const double y0 = asReal(y);
        return x0 < y0;
      }
        break;
        return NA_INTEGER;
      }
    }
  }
  return NA_INTEGER;
}

SEXP test_scalar_xoy(SEXP x, SEXP o, SEXP y) {
  const int op = do_op2M(o);
  int ans = scalar_xoy(x, op, y);
  if (ans != TRUE) {
    return ScalarLogical(0);
  }
  return ScalarLogical(1);
}

SEXP Cand2s(SEXP x1, SEXP op1, SEXP y1,
              SEXP x2, SEXP op2, SEXP y2,
              SEXP A, SEXP B,
              SEXP nThread, 
              SEXP depth) {
  // Return NULL if not separated -- revert to expr1 & expr2
  if (TYPEOF(depth) != INTSXP ||
      TYPEOF(nThread) != INTSXP) {
    return R_NilValue;
  }
  int idepth = asInteger(depth);
  if (idepth < -100 || idepth > 100) {
    Rprintf("idepth = %d\n", idepth);
  }
  if (idepth > 2) {
    return R_NilValue;
  }
  
  if (TYPEOF(A) == LGLSXP && TYPEOF(B) == LGLSXP) {
    return(do_and_lgl_lgl(A, B, nThread));
  }
  ///const int o1 = do_op2M(op1);
  ///const int o2 = do_op2M(op2);
  if (TYPEOF(A) == NILSXP && TYPEOF(B) == NILSXP) {
    R_xlen_t N = xlength(x1);
    if (N == 0 || xlength(y1) == 0 || xlength(x2) == 0 || xlength(y2) == 0) {
      // Don't contemplate 0-length results
      return R_NilValue;
    }
    if (N == 1) {
      if (xlength(x2) == 1 || true) {
        // Just evaluate at R level
        // Will be slower but no-one would care
        return R_NilValue;
      } else {
        // must be length > 1 so just reverse.
        ++idepth;
        SEXP new_depth = PROTECT(ScalarInteger(idepth));
        SEXP ans = PROTECT(Cand2s(x2, op2, y2,
                                    x1, op1, y1, 
                                    A, B,
                                    nThread, 
                                    new_depth));
        UNPROTECT(2);
        return ans;
      }
    }
    // The only permissible lengths are the length of x1
    // and length 1.  Note that our logic excludes exprs
    // like 1 == x but, simply put, this is unusual.
    if (xlength(y1) != N && xlength(y1) != 1) {
      return R_NilValue;
    }
    if (xlength(x2) != N && xlength(x2) != 1) {
      return R_NilValue;
    }
    if (xlength(y2) != N && xlength(y2) != 1) {
      return R_NilValue;
    }
    
    // if the second expression is back-to-front, i.e. 
    // 
    //  X1 <op1> Y1  &  x2 <op2> Y2
    // 
    // reverse it
    if (false && xlength(x2) == 1 && xlength(y2) > 1) {
      
      ++idepth;
      SEXP new_depth = PROTECT(ScalarInteger(idepth));
      SEXP ans = PROTECT(Cand2s(x1, op1, y1, 
                                  y2, op2, x2,   // y2 > 1
                                  A, B,
                                  nThread, 
                                  new_depth));
      UNPROTECT(2);
      return ans;
    }
    
    
    // Here we'll use uppercase to denote length-N vectors
    // and lowercase to represent scalars.  At this point,
    // we know the only combinations are
    //   X1 <op1> Y1  &  x2 <op2> Y2   // excluded above
    //   X1 <op1> Y1  &  x2 <op2> y2   // scalar RHS
    //   X1 <op1> Y1  &  X2 <op2> Y2
    //   X1 <op1> Y1  &  X2 <op2> y2
    //
    //   X1 <op1> y1  &  x2 <op2> Y2   // excluded above
    //   X1 <op1> y1  &  x2 <op2> y2   // scalar RHS
    //   X1 <op1> y1  &  X2 <op2> Y2
    //   X1 <op1> y1  &  X2 <op2> y2
    //
    // Let's deal with the scalar RHS 
    /// const int o1 = do_op2M(op1);
    
    
    if (TYPEOF(x1) == INTSXP && TYPEOF(y1) == INTSXP &&
        TYPEOF(x2) == INTSXP && TYPEOF(y2) == INTSXP) {
      return(do_and_int_int(x1, op1, y1,
                            x2, op2, y2, 
                            nThread));
    }
    if (TYPEOF(x1) == INTSXP && TYPEOF(y1) == INTSXP &&
        TYPEOF(x2) == INTSXP && TYPEOF(y2) == REALSXP) {
      
    }
    
  }
  return R_NilValue;
}



