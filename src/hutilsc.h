#ifndef hutilsc_H
#define hutilsc_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>

#if _OPENMP
#include <omp.h>
#endif

#define DEBUG 1

// int op = !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));
// != == >= <=  >  <
//  0  1  2  3  4  5
#define OP_NE 1
#define OP_EQ 2
#define OP_GE 3
#define OP_LE 4
#define OP_GT 5
#define OP_LT 6
#define OP_IN 7
#define OP_BW 8
#define OP_BO 9
#define OP_BC 10


#define return_false do {                                      \
            SEXP ans = PROTECT(allocVector(LGLSXP, 1));        \
            LOGICAL(ans)[0] = FALSE;                           \
            UNPROTECT(1);                                      \
            return ans;                                        \
} while (0)

#define return_true do {                                      \
SEXP ans = PROTECT(allocVector(LGLSXP, 1));                    \
LOGICAL(ans)[0] = TRUE;                                       \
UNPROTECT(1);                                                  \
return ans;                                                    \
} while (0)                                                    \

int do_op2M(SEXP op);

float ssqrt_fast(float x);
int radix_find(const int * k1p, const int a, int x0, int x1, int N);
int linear_find(const int * k1, const int a, const int N);
int linear_find_from(const int * k1, const int a, int from, const int N);
void radix_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N);
void linear_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N);
  
#endif
