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

#if defined _OPENMP && _OPENMP >= 201511
#define pragma_omp _Pragma("omp parallel for num_threads(nThread)")
#else
#define pragma_omp 
#endif




#define DEBUG 0

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

extern int tens[10];

// character
bool all_digits_4_12(const char * xi);
bool all_digits(const char * xi, size_t nchari);
int char2int(const char * x, int s);
int char12_to_int(const char * x);
int nth_digit_of(int x, int n);
unsigned char nth_char(int x, int n);
char digit2char(int d);
int n_digits0(unsigned int x);


int do_op2M(SEXP op);

// character
SEXP do_pad0(SEXP x, const int w);

// error
bool notInt(SEXP x);
bool notDbl(SEXP x);
int notEquiLgl2(SEXP x, SEXP y);
int notEquiInt2(SEXP x, SEXP y);
int notEquiInt3(SEXP x, SEXP y, SEXP z);
int notEquiDbl3(SEXP x, SEXP y, SEXP z);
int notEquiDbl2(SEXP x, SEXP y);

float ssqrt_fast(float x);
double euclid_dist_d(double d0, double d1);
double euclid_dist(double x0, double y0, double x1, double y1);

unsigned int radix_find(int a, unsigned int x0, unsigned int x1, const int * k1, unsigned int * tbl);
void radix_find_range(int a, 
                      const int * k1,
                      unsigned int * tbl, 
                      unsigned int N,
                      unsigned int * R);
void linear_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N);
void ftc2(int * U0, int * U1, const int * k1, int N);
int binary_find(int key, int * xp, int N)

// maxmin
unsigned int amax(unsigned int x[], int n);
int sxp_required(unsigned int x[], int n);
int maxXY(const int * x, const int * y, R_xlen_t Nx, R_xlen_t Ny, bool sx, bool sy);
void Vminmax_i(int minmax[], const int * x, R_xlen_t N, int nthreads);
SEXP Cminmax(SEXP x, SEXP emptyResult, SEXP nThread);

// pcg_hash
unsigned int pcg_sample1(unsigned int max);
unsigned int pcg_sample_halfmax();
unsigned int tpcg_sample_halfmax(int thread);

// ScalarLength
SEXP ScalarLength(R_xlen_t o);

// sortedness
bool sorted_int(const int * xp, R_xlen_t N, int nThreads);

// omp_diagnose
int as_nThread(SEXP x);
  
#endif
