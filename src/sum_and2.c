#include "hutilsc.h"


SEXP sum_and2_lgl_lgl(SEXP A, SEXP B, SEXP na_rm, SEXP nThread) {
  const int nThreads = asInteger(nThread);
  const bool rm_na = asLogical(na_rm);
  if (TYPEOF(A) == LGLSXP && TYPEOF(B) == LGLSXP) {
    R_xlen_t N = xlength(A);
    if (N == 0 || xlength(B) == 0) {
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = 0;
      UNPROTECT(1);
      return ans;
    }
    if (N == 1) {
      if (xlength(B) > 1) {
        return sum_and2_lgl_lgl(B, A, na_rm, nThread);
      } else {
        SEXP ans = PROTECT(allocVector(INTSXP, 1));
        INTEGER(ans)[0] = (int)asLogical(A) & (int)asLogical(B);
        UNPROTECT(1);
        return ans;
      }
    }
    if (xlength(B) == 1) {
      const int b = asLogical(B);
      if (b == NA_INTEGER) {
        SEXP ans = PROTECT(allocVector(INTSXP, 1));
        INTEGER(ans)[0] = rm_na ? 0 : NA_INTEGER ;
        UNPROTECT(1);
        return ans;
        
      } else if (b) {
        
        const int * ap = LOGICAL(A);
        if (N < INT_MAX) {
          // must be integer so we save time by just doing integer addition
          int o = 0;
          if (rm_na) {
            for (R_xlen_t i = 0; i < N; ++i) {
              int api = ap[i];
              o += api == 1;
            }
          } else {
            for (R_xlen_t i = 0; i < N; ++i) {
              int api = ap[i];
              if (api == NA_INTEGER) {
                o = NA_INTEGER;
                break;
              }
              o += api;
            }
          }
        } else {
          uint64_t o = 0;
          if (rm_na) {
            for (R_xlen_t i = 0; i < N; ++i) {
              int api = ap[i];
              o += api == 1;
            }
          } else {
            bool ever_na = false;
            for (R_xlen_t i = 0; i < N; ++i) {
              int api = ap[i];
              if (api == NA_INTEGER) {
                ever_na = true;
                break;
              }
              o += api;
            }
            if (ever_na) {
              SEXP ans = PROTECT(allocVector(INTSXP, 1));
              INTEGER(ans)[0] = NA_INTEGER;
              UNPROTECT(1);
              return ans;
            }
          }
          if (o >= INT_MAX) {
            // must be double
            SEXP ans = PROTECT(allocVector(REALSXP, 1));
            REAL(ans)[0] = o;
            UNPROTECT(1);
            return ans;
          } else {
            SEXP ans = PROTECT(allocVector(INTSXP, 1));
            INTEGER(ans)[0] = o;
            UNPROTECT(1);
            return ans;
          }
        }
      } else {
        // B is just FALSE so the sum of (A & B) is always 0
        SEXP ans = PROTECT(allocVector(INTSXP, 1));
        INTEGER(ans)[0] = 0;
        UNPROTECT(1);
        return ans;
      }
    }
    // xlength(b) == N
    if (xlength(B) != N) {
      return R_NilValue;
    }
    uint64_t o = 0;
    const int * ap = LOGICAL(A);
    const int * bp = LOGICAL(B);
    bool ever_na = false;
    if (rm_na) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(+ : o) 
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        int api = ap[i];
        int bpi = bp[i];
        o += api & bpi;
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThreads) reduction(+ : o) reduction(|| : ever_na)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        int api = ap[i];
        int bpi = bp[i];
        if (api == NA_INTEGER || bpi == NA_INTEGER) {
          ever_na = true;
        } else {
          o += api & bpi;
        }
      }
    }
    if (ever_na) {
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = NA_INTEGER;
      UNPROTECT(1);
      return ans;
    }
    if (o < INT_MAX) {
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = (int)o;
      UNPROTECT(1);
      return ans;
    } else {
      SEXP ans = PROTECT(allocVector(REALSXP, 1));
      REAL(ans)[0] = (double)o;
      UNPROTECT(1);
      return ans;
    }
    
    
  }
  return R_NilValue;
}


SEXP sum_and2(SEXP x1, SEXP op1, SEXP y1, 
              SEXP x2, SEXP op2, SEXP y2,
              SEXP A, SEXP B,
              SEXP na_rm,
              SEXP nThread) {
  if (TYPEOF(A) == LGLSXP && TYPEOF(B) == LGLSXP) {
    return sum_and2_lgl_lgl(A, B, na_rm, nThread);
  }
  if (TYPEOF(A) != LGLSXP && TYPEOF(B) == LGLSXP) {
    return sum_and2(x1, op1, y1,
                    x2, op2, y2,
                    B, A, 
                    na_rm,
                    nThread);
  }
  return R_NilValue;
}

